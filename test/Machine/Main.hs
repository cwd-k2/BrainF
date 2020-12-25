{-# LANGUAGE OverloadedLists #-}

module Main where

import           Control.Monad.State
import qualified Data.Vector.Unboxed as V
import qualified Machine             as M
import           System.IO           (BufferMode (..), hSetBuffering, stdin)

helloWorld = V.fromList $ unlines [ "+++++++++"
                                  , "[>++++++++>+++++++++++>+++>+<<<<-]"
                                  , ">.>++.+++++++..+++."
                                  , ">+++++.<<+++++++++++++++.>."
                                  , "+++.------.--------.>+.>+."]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  evalStateT (M.run helloWorld 0) M.initial

