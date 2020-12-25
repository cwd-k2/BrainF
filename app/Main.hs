module Main where

import           Control.Monad.State
import           Data.Maybe          (isJust)
import qualified Data.Vector.Unboxed as V
import qualified Machine             as M
import           System.Environment  (getArgs, lookupEnv)
import           System.IO

main :: IO ()
main = do
  noBuffering <- isJust <$> lookupEnv "BF_NO_BUFFERING"

  when noBuffering $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

  args <- getArgs

  when (null args) $ do
    error "Args too short. Abort."

  let filename = head args

  handle   <- openFile filename ReadMode
  contents <- hGetContents handle

  let program = V.fromList contents
  evalStateT (M.run program 0) M.initial

  hClose handle

