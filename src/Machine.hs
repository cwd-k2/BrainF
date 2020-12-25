module Machine
  ( Machine
  , initial
  , run
  ) where

import           Control.Monad.State
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import           Memory              (Memory)
import qualified Memory              as M

-- | メモリ, スタック (直前の '[' の位置のリスト)
type Machine = (Memory, [Int])

-- | 初期状態
initial :: Machine
initial = (M.initial, [])

run :: Vector Char -> Int -> StateT Machine IO ()
run v p
  | p == l    = return ()
  | otherwise = switch c p >>= run v
  where
    c = v ! p
    l = V.length v

switch :: Char -> Int -> StateT Machine IO Int
switch '>' = next
switch '<' = back
switch '+' = increment
switch '-' = decrement
switch '[' = push
switch ']' = jump
switch '.' = output
switch ',' = commit
switch _   = return . (+ 1)

updateMemoryWith :: (Memory -> Memory) -> StateT Machine IO ()
updateMemoryWith f = do
  (mem, stack) <- get
  put (f mem, stack)

next :: Int -> StateT Machine IO Int
next p = do
  updateMemoryWith M.nex
  return $ p + 1

back :: Int -> StateT Machine IO Int
back p = do
  updateMemoryWith M.pre
  return $ p + 1

increment :: Int -> StateT Machine IO Int
increment p = do
  updateMemoryWith M.inc
  return $ p + 1

decrement :: Int -> StateT Machine IO Int
decrement p = do
  updateMemoryWith M.dec
  return $ p + 1

push :: Int -> StateT Machine IO Int
push p = do
  (mem, stack) <- get
  put (mem, p:stack)
  return $ p + 1

jump :: Int -> StateT Machine IO Int
jump p = do
  (mem, q:stack) <- get
  put (mem, stack)
  return $ if M.cur mem == 0 then p + 1 else q

output :: Int -> StateT Machine IO Int
output p = do
  (mem, stack) <- get
  liftIO . putChar $ M.cur mem
  return $ p + 1

commit :: Int -> StateT Machine IO Int
commit p = do
  (mem, stack) <- get
  char <- liftIO getChar
  put (M.ins mem char, stack)
  return $ p + 1

