module Main (main) where

import Control.Monad.IO.Class
import Data.Set
import HW3.Action
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)
import System.Console.Haskeline
import Text.Megaparsec.Error

main :: IO ()
main = runInputT defaultSettings loop
  where
   loop :: InputT IO ()
   loop = do
     minput <- getInputLine "hi> "
     case minput of
       Nothing -> return ()
       Just "quit" -> return ()
       Just input -> do
         case (parse input) of
           Left parseError -> outputStrLn $ show $ errorBundlePretty parseError
           Right parsed -> do
             res <- liftIO $ runHIO (eval parsed) (fromList [AllowRead, AllowWrite, AllowTime])
             case res of
               Left evalError -> outputStrLn $ show $ evalError
               Right val -> outputStrLn $ show $ prettyValue val
         loop