module Main where

import qualified BrainF.Machine as B
import qualified BrainF.Memory  as M
import qualified BrainF.Program as P

main :: IO ()
main = do
  let src = "+++++++++[>++++++++>+++++++++++>+++>+<<<<-]>.>++.+++++++..+++.>+++++.<<+++++++++++++++.>.+++.------.--------.>+.>+."
  B.run getChar putChar (P.parse src) M.initial
  return ()

