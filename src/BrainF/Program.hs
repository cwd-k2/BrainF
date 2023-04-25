module BrainF.Program
  ( Program
  , Instruction(..)
  , parse
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

