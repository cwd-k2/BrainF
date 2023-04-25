module Main where

import           Control.Monad      (when)
import           Data.Maybe         (isJust)
import           System.Environment (getArgs, lookupEnv)
import           System.IO

import qualified BrainF.Machine     as B
import qualified BrainF.Memory      as M
import qualified BrainF.Program     as P

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

  B.run getChar putChar (P.parse contents) M.initial

  hClose handle

  return ()
