{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}

module HW3.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Reader (ReaderT(ReaderT), ask, when, liftIO, MonadReader, MonadIO)
import Data.Set (Set, member)
import qualified Data.ByteString as BS
import qualified Data.Sequence as SQ
import qualified Data.Text as T
import qualified Data.Time as Time
import System.Directory
import System.Random
import HW3.Base

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT (Set HiPermission) IO)
  deriving (MonadReader (Set HiPermission)) via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction action = do
    checkPermission action
    case action of
      -- T7
      (HiActionRead fp) -> actionRead fp
      (HiActionWrite fp s) -> voidAction $ BS.writeFile fp s
      (HiActionMkDir fp) -> voidAction $ createDirectoryIfMissing True fp
      (HiActionChDir fp) -> voidAction $ do
        isDirectory <- liftIO $ doesDirectoryExist fp
        if isDirectory
        then setCurrentDirectory fp
        else return ()
      HiActionCwd -> abstractAction (HiValueString . T.pack) getCurrentDirectory
      -- T8
      HiActionNow -> abstractAction HiValueTime Time.getCurrentTime
      -- T9
      (HiActionRand x y) -> abstractAction (HiValueNumber . toRational) (getStdRandom $ uniformR (x, y))
      -- T10
      (HiActionEcho s) -> voidAction (putStrLn $ T.unpack s)

-- | if filepath is incorrect (is neither an existing directory nor an existing file)
-- returns null
-- if fp is file, reads its content
-- if fp is directory, returns its listing
actionRead :: FilePath -> HIO HiValue
actionRead fp = do
  isFile <- liftIO $ doesFileExist fp
  isDirectory <- liftIO $ doesDirectoryExist fp
  liftIO $ if isFile then do
    x <- liftIO $ BS.readFile fp
    let decoded = decodeUtf8Bytes x
    pure $ if decoded == HiValueNull then HiValueBytes x else decoded
  else do
    if (not isDirectory)
    then return HiValueNull
    else do
      l <- listDirectory fp
      pure $ HiValueList $ SQ.fromList (HiValueString . T.pack <$> l)

-- | performs a "void" action and returns null
voidAction :: IO () -> HIO HiValue
voidAction = abstractAction $ const HiValueNull

-- | performs an action and wraps the result
abstractAction :: (a -> HiValue) -> IO a -> HIO HiValue
abstractAction resultWrapper action = liftIO action >>= (return . resultWrapper)

-- | UTILS | --
permission :: HiAction -> Maybe HiPermission
permission = \case
  HiActionRead _ -> Just AllowRead
  HiActionWrite _ _ -> Just AllowWrite
  HiActionMkDir _ -> Just AllowWrite
  HiActionChDir _ -> Just AllowRead
  HiActionCwd -> Just AllowRead
  HiActionNow -> Just AllowTime
  HiActionEcho _ -> Just AllowWrite
  _ -> Nothing

-- | throws PermissionException if the action isn't allowed
checkPermission :: HiAction -> HIO ()
checkPermission action = case (permission action) of
  Just p -> do
    perms <- ask
    liftIO $ when (not $ member p perms) $
      throwIO $ PermissionRequired p
  _ -> pure ()