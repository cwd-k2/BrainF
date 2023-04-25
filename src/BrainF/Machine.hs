module BrainF.Machine
  ( run
  ) where

import           Control.Monad  (foldM)

import           BrainF.Memory  (Memory)
import qualified BrainF.Memory  as M
import           BrainF.Program (Instruction (..), Program)

-- うーん微妙
eval :: Monad m
     => m Char
     -> (Char -> m ())
     -> Instruction
     -> Memory
     -> m Memory
eval _ _ Increment m = return $ M.increment m
eval _ _ Decrement m = return $ M.decrement m
eval _ _ Forward   m = return $ M.step m
eval _ _ Backward  m = return $ M.back m
eval i o (Loop p) m
  | M.current m /= toEnum 0 = run i o p m >>= eval i o (Loop p)
  | otherwise               = return m
eval i _ Input m     = do
  c <- i
  return $ M.replace c m
eval _ o Output m    = do
  o (M.current m)
  return m

run :: Monad m
    => m Char
    -> (Char -> m ())
    -> Program
    -> Memory
    -> m Memory
run i o p m = foldM (flip $ eval i o) m p

