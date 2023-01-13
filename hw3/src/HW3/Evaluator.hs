{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module HW3.Evaluator (eval, EM') where

import Codec.Compression.Zlib (defaultDecompressParams, compressLevel, bestCompression,
                               defaultCompressParams, compressWith, decompressWith)
import qualified Codec.Serialise as SR
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)
import Control.Monad.Trans.Except ( ExceptT (..), runExceptT, throwE)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Semigroup (stimes)
import qualified Data.List as LS
import qualified Data.Sequence as SQ
import qualified Data.Text as T
import qualified Data.Map as MP
import Data.Text.Encoding (encodeUtf8, encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime)
import Data.Word (Word8)
import GHC.Real (Ratio ((:%)))
import Text.Read (readMaybe)
import HW3.Base

type EM m = ExceptT HiError m HiValue
type EM' m a = ExceptT HiError m a

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eval'

eval' :: HiMonad m => HiExpr -> EM m
eval' = \case
  HiExprValue val -> return val
  HiExprApply expr args -> eval' expr >>= evalApplication args
  HiExprRun expr -> eval' expr >>= evalRun
  HiExprDict pairs -> do
    values <- mapM evalPair (toList pairs)
    return $ HiValueDict (MP.fromList values)
    where
      evalPair (x, y) = do
        x' <- eval' x
        y' <- eval' y
        return (x', y')

evalApplication :: HiMonad m => [HiExpr] -> HiValue -> EM m
evalApplication args func = do
  checkArity (length args) func
  case func of
    -- list as function
    HiValueString s -> stringIndexSlice s args
    HiValueList l -> listIndexSlice l args
    HiValueBytes b -> bytesIndexSlice b args
    -- dot access
    HiValueDict d -> mapM eval' args >>= dotAccess d
    -- function
    HiValueFunction f -> case (hiFunType f) of
      BoolLazy -> evalBoolLazy f args
      _ -> mapM eval' args >>= evalOperation f
    -- non-function
    _ -> throwE HiErrorInvalidFunction

-- | evaluates all actions
evalRun :: HiMonad m => HiValue -> EM m
evalRun (HiValueAction ac) = lift $ runAction ac
evalRun _ = throwE HiErrorInvalidArgument

-- | takes a function and evaluated arguments and applies the function.
-- throws:
--  HiErrorInvalidFunction if numbers are used in function positions
--  HiErrorDivideByZero
--  HiErrorInvalidArgument if the function can't be applied to the passed arguments
evalOperation :: HiMonad m => HiFun -> [HiValue] -> EM m
evalOperation hiFun args = case (hiFunType hiFun) of
  -- T1
  BinaryArithmetic -> evalBinaryArithmetic args hiFun
  -- T2
  BinaryBool -> evalBinaryBool hiFun args
  BoolLazy -> evalBoolLazyButNotActuallyLazy args hiFun
  -- T4
  UnaryText -> evalUnaryText hiFun args
  -- T5
  List -> evalListOp args hiFun
  -- T6
  Bytes -> evalBytes args hiFun
  -- T7
  FileIO -> evalFileIO args hiFun
  -- T11
  Dict -> evalDict args hiFun
  Other -> case hiFun of
    HiFunNot -> evalNot args
    HiFunReverse -> evalReverse args
    HiFunLength -> evalLength args
    -- T8
    HiFunParseTime -> evalParseTime args
    -- T9
    HiFunRand -> evalRand args
    -- T10
    HiFunEcho -> evalEcho args
    HiFunCount -> evalCount args
    _ -> throwE HiErrorInvalidFunction

-- | T1 :: NUMBERS AND ARITHMETIC |
evalBinaryArithmetic :: HiMonad m => [HiValue] -> HiFun -> EM m
-- all arithmetic operations with numbers
evalBinaryArithmetic [(HiValueNumber _), (HiValueNumber 0)] HiFunDiv = throwE HiErrorDivideByZero
evalBinaryArithmetic [(HiValueNumber x), (HiValueNumber y)] op = do
  f <- getBinaryArithmeticOp op
  returnNumber $ f x y
-- with strings
evalBinaryArithmetic [(HiValueString x), (HiValueString y)] HiFunDiv = returnString $ (x <> T.pack "/" <> y)
evalBinaryArithmetic [(HiValueString x), (HiValueString y)] HiFunAdd = returnString $ x <> y
evalBinaryArithmetic [(HiValueString x), (HiValueNumber p)] HiFunMul = HiValueString <$> stimes' p x
-- with lists
evalBinaryArithmetic [(HiValueList x), (HiValueList y)] HiFunAdd = returnList $ x <> y
evalBinaryArithmetic [(HiValueList x), (HiValueNumber p)] HiFunMul =  HiValueList <$> stimes' p x
-- with bytes
evalBinaryArithmetic [(HiValueBytes x), (HiValueBytes y)] HiFunAdd = returnBytes $ x <> y
evalBinaryArithmetic [(HiValueBytes x), (HiValueNumber p)] HiFunMul = HiValueBytes <$> stimes' p x
-- with time
evalBinaryArithmetic [(HiValueTime t), (HiValueNumber n)] HiFunAdd = return $ HiValueTime $ addUTCTime (fromRational n) t
evalBinaryArithmetic [(HiValueTime t1), (HiValueTime t2)] HiFunSub = returnNumber $ toRational (diffUTCTime t1 t2)
-- arithmetic operations with other types of arguments aren't supported
evalBinaryArithmetic _ _ = throwE HiErrorInvalidArgument


-- | T2 :: BOOLEANS AND COMPARISON |
-- | all comparison operations
evalBinaryBool :: HiMonad m => HiFun -> [HiValue] -> EM m
evalBinaryBool hiFun [x, y] = do
  f <- getBinaryBoolOp hiFun
  returnBool $ f x y
evalBinaryBool _ _ = throwE HiErrorInvalidArgument

-- | T10 - lazy logic - and / or / if
evalBoolLazy :: HiMonad m => HiFun -> [HiExpr] -> EM m
evalBoolLazy HiFunIf [pr, x, y] = do
  pred' <- eval' pr
  case pred' of
    (HiValueBool res) -> if res then eval' x else eval' y
    _ -> throwE HiErrorInvalidArgument
evalBoolLazy HiFunAnd [x, y] = evalAndOr True x y
evalBoolLazy HiFunOr [x, y] = evalAndOr False x y
evalBoolLazy _ _ = throwE HiErrorInvalidArgument

-- | we need this to support expressions like fold(and, [false,true,false])
evalBoolLazyButNotActuallyLazy :: HiMonad m => [HiValue] -> HiFun -> EM m
evalBoolLazyButNotActuallyLazy [HiValueBool x, HiValueBool y] f = case f of
  HiFunAnd -> returnBool $ x && y
  HiFunOr -> returnBool $ x || y
  _ -> throwE HiErrorInvalidFunction
evalBoolLazyButNotActuallyLazy [(HiValueBool predicate), x, y] HiFunIf = return $ if predicate then x else y
evalBoolLazyButNotActuallyLazy _ _ = throwE HiErrorInvalidArgument

-- a (not very beautiful) helper function to avoid copy-paste
evalAndOr :: HiMonad m => Bool -> HiExpr -> HiExpr -> EM m
evalAndOr isAnd x y = do
  x' <- eval' x
  if isNullOrFalse x' == isAnd then return x' else eval' y
    where
      isNullOrFalse = \case
        HiValueNull -> True
        (HiValueBool False) -> True
        _ -> False

-- | not
evalNot :: HiMonad m => [HiValue] -> EM m
evalNot [(HiValueBool x)] = returnBool $ not x
evalNot _ = throwE HiErrorInvalidArgument

-- | T4 :: STRINGS AND SLICES |
-- index and slice
stringIndexSlice :: HiMonad m => T.Text -> [HiExpr] -> EM m
stringIndexSlice s = listAsFunction (T.unpack s) (HiValueString . T.singleton) (HiValueString . T.pack)

-- | reverse
evalReverse :: HiMonad m => [HiValue] -> EM m
evalReverse = \case
  [(HiValueList l)] -> returnList $ SQ.reverse l
  [(HiValueString s)] -> returnString $ T.reverse s
  _ -> throwE HiErrorInvalidArgument

-- | length
evalLength :: HiMonad m => [HiValue] -> EM m
evalLength = \case
  [(HiValueList l)] -> returnNumber $ toRational $ SQ.length l
  [(HiValueString s)] -> returnNumber $ toRational $ T.length s
  _ -> throwE HiErrorInvalidArgument

-- | toUpper, toLower, trim
evalUnaryText :: HiMonad m => HiFun -> [HiValue] -> EM m
evalUnaryText hiFun [(HiValueString str)] = do
  f <- getUnaryTextOp hiFun
  returnString $ f str
evalUnaryText _ _ = throwE HiErrorInvalidArgument


-- | T5 :: LISTS AND FOLDS |
-- | index and slice
listIndexSlice :: HiMonad m => SQ.Seq HiValue -> [HiExpr] -> EM m
listIndexSlice l = listAsFunction (Data.Foldable.toList l) id (HiValueList . SQ.fromList)

-- | list, range, fold
evalListOp :: HiMonad m => [HiValue] -> HiFun -> EM m
evalListOp args = \case
  HiFunList -> returnList $ SQ.fromList args
  HiFunRange -> evalRange args
  HiFunFold -> evalFold args
  _ -> throwE HiErrorInvalidArgument

-- | range
evalRange :: HiMonad m => [HiValue] -> EM m
evalRange [(HiValueNumber l), (HiValueNumber r)] = returnList $ SQ.fromList $ fmap HiValueNumber [l..r]
evalRange _ = throwE HiErrorInvalidArgument

-- | fold
evalFold :: HiMonad m => [HiValue] -> EM m
evalFold [(HiValueFunction f), (HiValueList l)] = case l of
  [] -> return HiValueNull
  [val] -> return val
  els ->
    foldl1 inner (return <$> els)
    where
      inner a b = do
        x <- a
        y <- b
        evalOperation f [x, y]
evalFold _ = throwE HiErrorInvalidArgument


-- | T6 :: BYTES |
-- | index and slice
packBytes :: [Word8] -> HiValue
packBytes = HiValueBytes . BS.pack

bytesIndexSlice :: HiMonad m => BS.ByteString -> [HiExpr] -> EM m
bytesIndexSlice b = listAsFunction (unpackBytes b) id (packBytes . (fmap toInt))
  where
    toInt :: HiValue -> Word8
    toInt (HiValueNumber (x :% _)) = fromIntegral x
    toInt _ = undefined -- since we perform "bytesIndexSlice" operation on HiValueBytes,
    -- which were previously evaluated, we can be sure that the values are in correct format.

-- | all operations with bytes
evalBytes :: HiMonad m => [HiValue] -> HiFun -> EM m
evalBytes [(HiValueList l)] HiFunPackBytes = packBytes . toList <$> mapM toByte l
evalBytes [(HiValueString s)] HiFunEncodeUtf8 = returnBytes $ encodeUtf8 s
evalBytes [el] HiFunSerialise = returnBytes . BS.Lazy.toStrict $ SR.serialise el
evalBytes [(HiValueBytes bytes)] hiFun = case hiFun of
  HiFunUnpackBytes -> returnList $ SQ.fromList $ unpackBytes bytes
  HiFunDecodeUtf8 -> return $ decodeUtf8Bytes bytes
  HiFunZip -> returnBytes $ toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } (fromStrict bytes)
  HiFunUnzip -> returnBytes $ toStrict $ decompressWith defaultDecompressParams (fromStrict bytes)
  HiFunDeserialise -> return $ SR.deserialise $ fromStrict bytes
  _ -> throwE HiErrorInvalidFunction
evalBytes _ _ = throwE HiErrorInvalidArgument

-- | translates number to byte
toByte :: HiMonad m => HiValue -> EM' m Word8
toByte = \case
  (HiValueNumber (x :% q))
    | x >= 0 && x <= 255 && q == 1 -> return $ fromIntegral x
    | otherwise -> throwE HiErrorInvalidArgument
  _ -> throwE HiErrorInvalidArgument

unpackBytes :: BS.ByteString -> [HiValue]
unpackBytes s = fmap (HiValueNumber . toRational) $ BS.unpack s


-- | T7 FILE IO | --
evalFileIO :: HiMonad m => [HiValue] -> HiFun -> EM m
evalFileIO [(HiValueString file)] f = case f of
  HiFunRead -> returnAction $ HiActionRead $ T.unpack file
  HiFunMkDir -> returnAction $ HiActionMkDir $ T.unpack file
  HiFunChDir -> returnAction $ HiActionChDir $ T.unpack file
  _ -> throwE HiErrorInvalidFunction
evalFileIO [(HiValueString file), (HiValueString s)] HiFunWrite = returnAction $ HiActionWrite (T.unpack file) (encodeUtf8 s)
evalFileIO _ _ = throwE HiErrorInvalidArgument


-- | T8 TIME | --
evalParseTime :: HiMonad m => [HiValue] -> EM m
evalParseTime [(HiValueString s)] = return $ case (readMaybe (T.unpack s)) of
  Just t -> HiValueTime t
  _ -> HiValueNull
evalParseTime _ = throwE HiErrorInvalidArgument


-- | T9 RAND NUMBERS | --
evalRand :: HiMonad m => [HiValue] -> EM m
evalRand [(HiValueNumber x), (HiValueNumber y)] = do
  from <- toInt x
  to <- toInt y
  returnAction $ HiActionRand (fromInteger from) (fromInteger to)
    where
      toInt :: HiMonad m => Rational -> EM' m Integer
      toInt (p :% q)
        | q /= 1 = throwE HiErrorInvalidArgument
        | otherwise = return $ p
evalRand _ = throwE HiErrorInvalidArgument


-- | T10 ECHO | --
evalEcho :: HiMonad m => [HiValue] -> EM m
evalEcho [(HiValueString s)] = returnAction $ HiActionEcho s
evalEcho _ = throwE HiErrorInvalidArgument

-- | T11 DICT | --
evalDict :: HiMonad m => [HiValue] -> HiFun -> EM m
evalDict [(HiValueDict d)] f = case f of
  HiFunKeys -> returnList $ SQ.fromList $ MP.keys d
  HiFunValues -> returnList $ SQ.fromList $ MP.elems d
  HiFunInvert -> returnDict $ MP.map (HiValueList . SQ.fromList) (invert d)
  _ -> throwE HiErrorInvalidFunction
  where
    invert :: Ord v => MP.Map k v -> MP.Map v [k]
    invert m = MP.fromListWith (++) [(v, [k]) | (k, v) <- MP.toList m]
evalDict _ _ = throwE HiErrorInvalidArgument

evalCount :: HiMonad m => [HiValue] -> EM m
evalCount [el] = case el of
  (HiValueString s) -> returnDict $ MP.mapKeys (HiValueString . T.singleton) (MP.map HiValueNumber (count $ T.unpack s))
  (HiValueList l) -> returnDict $ MP.map HiValueNumber (count $ toList l)
  (HiValueBytes b) -> returnDict $ MP.mapKeys (HiValueNumber . toRational . toInteger) (MP.map HiValueNumber (count $ BS.unpack b))
  _ -> throwE HiErrorInvalidArgument
  where
    count :: Ord k => [k] -> MP.Map k Rational
    count s = MP.fromListWith (+) [(c, 1) | c <- s]
evalCount _ = throwE HiErrorInvalidArgument

dotAccess :: HiMonad m => MP.Map HiValue HiValue -> [HiValue] -> EM m
dotAccess dict [el] = do
  let res = MP.lookup el dict
  case res of
    Nothing -> return HiValueNull
    (Just res') -> return res'
dotAccess _ _ = throwE HiErrorInvalidArgument

-- | HELPERS | --
-- | slice and index
-- For values used as a function of one argument (String | List | Bytes).
-- Performs slice or index operation depending on the passed arguments.
listAsFunction :: HiMonad m => [a] -> (a -> HiValue) -> ([a] -> HiValue) -> [HiExpr] -> EM m
listAsFunction list elementsWrapper listWrapper args = do
  evaluatedArgs <- mapM eval' args
  case evaluatedArgs of
    [HiValueNumber ind] -> return $ listGetByIndex ind list elementsWrapper
    [HiValueNull, HiValueNull] -> getSlice 0 listLength
    [HiValueNumber (from :% _), HiValueNull] -> getSlice from listLength
    [HiValueNull, HiValueNumber (to :% _)] -> getSlice 0 to
    [HiValueNumber (from :% _), HiValueNumber (to :% _)] -> getSlice from to
    _ -> throwE HiErrorInvalidArgument
    where
      listLength = fromIntegral $ length list
      getSlice from' to' = return $ listWrapper $ take ((transformBorders to') - (transformBorders from')) (drop (transformBorders from') list)
      transformBorders :: Integer -> Int
      transformBorders x = fromIntegral $ if x < 0 then listLength + x else x

-- | list index
-- only works with natural numbers (zero included), otherwise or if index is greater than list length returns null
listGetByIndex :: Rational -> [a] -> (a -> HiValue) -> HiValue
listGetByIndex (p :% q) list resultWrapper =
  let i = fromIntegral p in
    if (q /= 1 || i < 0 || i >= LS.length list)
    then HiValueNull
    else resultWrapper $ list !! i

-- | checks if the number of arguments passed matches the arity of the function
-- and throws HiErrorArityMismatch otherwise
checkArity :: HiMonad m => Int -> HiValue -> EM' m ()
checkArity argsCount = \case
  HiValueString _ -> checkListAsFunctionArgs argsCount
  HiValueList _ -> checkListAsFunctionArgs argsCount
  HiValueBytes _ -> checkListAsFunctionArgs argsCount
  HiValueDict _ -> compareArityToExpected argsCount 1
  HiValueFunction f -> case hiFunArity f of
    Unary -> compareArityToExpected argsCount 1
    Binary -> compareArityToExpected argsCount 2
    Ternary -> compareArityToExpected argsCount 3
    None -> pure ()
  _ -> throwE HiErrorInvalidFunction
  where
    checkListAsFunctionArgs :: HiMonad m => Int -> EM' m ()
    checkListAsFunctionArgs argsCount' = when (argsCount' /= 1 && argsCount' /= 2) $ throwE HiErrorArityMismatch
    compareArityToExpected :: HiMonad m => Int -> Int -> EM' m ()
    compareArityToExpected argsCount' expected = when (argsCount' /= expected) $ throwE HiErrorArityMismatch

-- | checks if the argument is valid (is integer and greater than zero) and performs stimes
stimes' :: (Semigroup a, HiMonad m) => Rational -> a -> EM' m a
stimes' (p :% q) x = if (q /= 1 || p <= 0)
  then throwE HiErrorInvalidArgument
  else return $ stimes p x

-- | returns
returnString :: HiMonad m => T.Text -> EM m
returnString = return . HiValueString

returnList :: HiMonad m => SQ.Seq HiValue -> EM m
returnList = return . HiValueList

returnBytes :: HiMonad m => BS.ByteString -> EM m
returnBytes = return . HiValueBytes

returnNumber :: HiMonad m => Rational -> EM m
returnNumber = return . HiValueNumber

returnBool :: HiMonad m => Bool -> EM m
returnBool = return . HiValueBool

returnAction :: HiMonad m => HiAction -> EM m
returnAction = return . HiValueAction

returnDict :: HiMonad m => MP.Map HiValue HiValue -> EM m
returnDict = return . HiValueDict