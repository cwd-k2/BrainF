module Main where

import qualified BrainF.Memory  as M
import qualified BrainF.Program as P

main :: IO ()
main = do
  let src = "+++++++++[>++++++++>+++++++++++>+++>+<<<<-]>.>++.+++++++..+++.>+++++.<<+++++++++++++++.>.+++.------.--------.>+.>+."
  P.run (P.parse src) M.initial
  return ()

