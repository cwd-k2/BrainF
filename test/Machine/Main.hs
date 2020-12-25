{-# LANGUAGE OverloadedLists #-}

module Main where

import           Control.Monad.State
import qualified Data.Vector.Unboxed as V
import qualified Machine             as M

helloWorld = V.fromList $ unlines [ "+++++++++"
                                  , "[>++++++++>+++++++++++>+++>+<<<<-]"
                                  , ">.>++.+++++++..+++."
                                  , ">+++++.<<+++++++++++++++.>."
                                  , "+++.------.--------.>+.>+."]

main :: IO ()
main = do
  evalStateT (M.run helloWorld 0) M.initial

