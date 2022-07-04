module BrainF.Program
  ( Program
  , Instruction(..)
  , parse
  , run
  ) where

import           Control.Monad          (foldM, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           BrainF.Memory          (Memory)
import qualified BrainF.Memory          as M

type Program
  = [Instruction]

data Instruction
  = Increment    -- ^ '+' : memory increment
  | Decrement    -- ^ '-' : memory decrement
  | Forward      -- ^ '>' : address forward
  | Backward     -- ^ '<' : address backward
  | Loop Program -- ^ '[' .. ']'
  | Input        -- ^ ',' : input char to current address
  | Output       -- ^ '.' : output current address value as char
  deriving Show

parse :: String -> Program
parse []         = []
parse ('+' : cs) = Increment : parse cs
parse ('-' : cs) = Decrement : parse cs
parse ('>' : cs) = Forward   : parse cs
parse ('<' : cs) = Backward  : parse cs
parse ('[' : cs) =
  let (p, q) = splitAt (loopEnd cs) cs
   in Loop (parse p) : parse q
parse (']' : cs) = parse cs
parse (',' : cs) = Input     : parse cs
parse ('.' : cs) = Output    : parse cs
parse (_   : cs) = parse cs

loopEnd :: String -> Int
loopEnd = go 0 1 where
  go i 0 _  = i - 1
  go i _ [] = undefined
  go i nest (c:cs)
    | c == '['  = go (i + 1) (nest + 1) cs
    | c == ']'  = go (i + 1) (nest - 1) cs
    | otherwise = go (i + 1) nest cs

eval :: MonadIO m => Instruction -> Memory -> m Memory
eval Increment m = return $ M.increment m
eval Decrement m = return $ M.decrement m
eval Forward   m = return $ M.step m
eval Backward  m = return $ M.back m
eval (Loop p) m
  | M.current m /= toEnum 0 = run p m >>= eval (Loop p)
  | otherwise               = return m
eval Input m     = do
  c <- liftIO getChar
  return $ M.replace c m
eval Output m    = do
  liftIO $ putChar (M.current m)
  return m

run :: MonadIO m => Program -> Memory -> m Memory
run p m = foldM (flip eval) m p

