{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HW3.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , HiAction (..)
  , hiFunArity
  , hiFunName
  , hiFunByName
  , hiFunNames
  , hiFunType
  , Arity (..)
  , FunType (..)
  , getBinaryArithmeticOp
  , getBinaryBoolOp
  , getUnaryTextOp
  , HiMonad (..)
  , decodeUtf8Bytes
  ) where

import Codec.Serialise (Serialise)
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import qualified Data.ByteString as BS
import Data.Map (Map, fromList, (!), keys)
import qualified Data.Sequence as SQ
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data HiFun =
  -- T1
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  -- T2
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  -- T4
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  -- T5
  | HiFunList
  | HiFunRange
  | HiFunFold
  -- T6
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  -- T7
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  -- T8
  | HiFunParseTime
  -- T9
  | HiFunRand
  -- T10
  | HiFunEcho
  -- T11
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Show, Generic, Serialise)

data HiValue =
  -- T2
  HiValueBool Bool
  -- T1
  | HiValueNumber Rational
  | HiValueFunction HiFun
  -- T4
  | HiValueNull
  | HiValueString Text
  -- T5
  | HiValueList (SQ.Seq HiValue)
  -- T6
  | HiValueBytes BS.ByteString
  -- T7
  | HiValueAction HiAction
  -- T8
  | HiValueTime UTCTime
  -- T10
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Ord, Show, Generic, Serialise)

data HiExpr =
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  -- T7
  | HiExprRun HiExpr
  -- T10
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Eq, Ord, Show)

data HiError =
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq)

data HiAction =
  -- T7
    HiActionRead  FilePath
  | HiActionWrite FilePath BS.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  -- T8
  | HiActionNow
  -- T9
  | HiActionRand Int Int
  -- T10
  | HiActionEcho Text
  deriving (Eq, Ord, Show, Generic, Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

-- NB: 'None' is the arity of functions which take an unfixed number of elements
data Arity = Unary | Binary | Ternary | None

data FunType =
  BinaryArithmetic -- +, *, /, -
  | BinaryBool     -- <, >, ==, /=, <=, >=
  | BoolLazy       -- if, &&, ||
  | UnaryText      -- toUpper, toLower, trim
  | Bytes          -- all operations with bytes
  | List           -- list, range, fold
  | FileIO         -- read, write, mkdir, cd
  | Dict
  | Other          -- length, reverse, parse-time, echo, rand
  -- perhaps it would be better to create more types instead of Other,
  -- but I figured that it would be an unnecessarily work in my case

-- | (function name, arity, function type)
type FunInfo = (String, Arity, FunType)

funInfo :: [(HiFun, FunInfo)]
funInfo = [
          -- T1
            (HiFunDiv, ("div", Binary, BinaryArithmetic))
          , (HiFunMul, ("mul", Binary, BinaryArithmetic))
          , (HiFunAdd, ("add", Binary, BinaryArithmetic))
          , (HiFunSub, ("sub", Binary, BinaryArithmetic))
          -- T2
          , (HiFunAnd, ("and", Binary, BoolLazy))
          , (HiFunOr, ("or", Binary, BoolLazy))
          , (HiFunLessThan, ("less-than", Binary, BinaryBool))
          , (HiFunGreaterThan, ("greater-than", Binary, BinaryBool))
          , (HiFunEquals, ("equals", Binary, BinaryBool))
          , (HiFunNotLessThan, ("not-less-than", Binary, BinaryBool))
          , (HiFunNotGreaterThan, ("not-greater-than", Binary, BinaryBool))
          , (HiFunNotEquals, ("not-equals", Binary, BinaryBool))
          , (HiFunNot, ("not", Unary, Other))
          , (HiFunIf, ("if", Ternary, BoolLazy))
          -- T4
          , (HiFunLength, ("length", Unary, Other))
          , (HiFunToUpper, ("to-upper", Unary, UnaryText))
          , (HiFunToLower, ("to-lower", Unary, UnaryText))
          , (HiFunReverse, ("reverse", Unary, Other))
          , (HiFunTrim, ("trim", Unary, UnaryText))
          -- T5
          , (HiFunList, ("list", None, List))
          , (HiFunRange, ("range", Binary, List))
          , (HiFunFold, ("fold", Binary, List))
          -- T6
          , (HiFunPackBytes, ("pack-bytes", Unary, Bytes))
          , (HiFunUnpackBytes, ("unpack-bytes", Unary, Bytes))
          , (HiFunEncodeUtf8, ("encode-utf8", Unary, Bytes))
          , (HiFunDecodeUtf8, ("decode-utf8", Unary, Bytes))
          , (HiFunZip, ("zip", Unary, Bytes))
          , (HiFunUnzip, ("unzip", Unary, Bytes))
          , (HiFunSerialise, ("serialise", Unary, Bytes))
          , (HiFunDeserialise, ("deserialise", Unary, Bytes))
          -- T7
          , (HiFunRead, ("read", Unary, FileIO))
          , (HiFunWrite, ("write", Binary, FileIO))
          , (HiFunMkDir, ("mkdir", Unary, FileIO))
          , (HiFunChDir, ("cd", Unary, FileIO))
          -- T8
          , (HiFunParseTime, ("parse-time", Unary, Other))
          -- T9
          , (HiFunRand, ("rand", Binary, Other))
          -- T10
          , (HiFunEcho, ("echo", Unary, Other))
          -- T11
          , (HiFunCount, ("count", Unary, Other))
          , (HiFunKeys, ("keys", Unary, Dict))
          , (HiFunValues, ("values", Unary, Dict))
          , (HiFunInvert, ("invert", Unary, Dict))
          ]

-- Okay, I know that pattern matching works faster than (!),
-- but I figured that since this map consists of the small amount of elements
-- and since this solution helps to avoid copy-paste, this is not a big deal.
-- It is also much easier to add new features this way. So...
-- Please have a mercy on me ^(
funToInfo :: Map HiFun FunInfo
funToInfo = fromList $ funInfo

-- | map [functionName, info]
nameToFun :: Map String HiFun
nameToFun = fromList $ [(name, hiFun) | (hiFun, (name, _, _)) <- funInfo]

-- a bunch of functions to get the required field
hiFunName :: HiFun -> String
hiFunName = sel1 . (funToInfo !)

hiFunArity :: HiFun -> Arity
hiFunArity = sel2 . (funToInfo !)

hiFunType :: HiFun -> FunType
hiFunType = sel3 . (funToInfo !)

hiFunByName :: String -> HiFun
hiFunByName = (nameToFun !)

-- list of all function names
hiFunNames :: [String]
hiFunNames = keys nameToFun

getBinaryArithmeticOp :: Monad m => HiFun -> ExceptT HiError m (Rational -> Rational -> Rational)
getBinaryArithmeticOp = \case
  HiFunDiv -> return $ (/)
  HiFunMul -> return $ (*)
  HiFunAdd -> return $ (+)
  HiFunSub -> return $ (-)
  _ -> throwE HiErrorInvalidFunction

getBinaryBoolOp :: Monad m => HiFun -> ExceptT HiError m (HiValue -> HiValue -> Bool)
getBinaryBoolOp = \case
  HiFunLessThan -> return $ (<)
  HiFunGreaterThan -> return $ (>)
  HiFunEquals -> return $ (==)
  HiFunNotLessThan -> return $ (>=)
  HiFunNotGreaterThan -> return $ (<=)
  HiFunNotEquals -> return $ (/=)
  _ -> throwE HiErrorInvalidFunction

getUnaryTextOp :: Monad m => HiFun -> ExceptT HiError m (Text -> Text)
getUnaryTextOp = \case
  HiFunToUpper -> return $ T.toUpper
  HiFunToLower -> return $ T.toLower
  HiFunTrim -> return $ T.strip
  _ -> throwE HiErrorInvalidFunction

decodeUtf8Bytes :: BS.ByteString -> HiValue
decodeUtf8Bytes bytes = case decodeUtf8' bytes of
  Left _ -> HiValueNull
  Right res -> HiValueString res