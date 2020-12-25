module Machine where

import           Control.Monad.State
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import           Memory              (Memory, cur, dec, inc, ins, nex, pre)
import qualified Memory              as M
import           System.IO           (hFlush, stdout)

-- | メモリ, スタック (直前の '[' の位置のリスト)
type Machine = (Memory , [Int])

initial :: Machine
initial = (M.initial, [])

run :: Vector Char -> Int -> StateT Machine IO ()
run v p | p == len  = return ()
        | c == '>'  = next p >>= run v
        | c == '<'  = back p >>= run v
        | c == '+'  = increment p >>= run v
        | c == '-'  = decrement p >>= run v
        | c == '['  = push p >>= run v
        | c == ']'  = jump p >>= run v
        | c == '.'  = output p >>= run v
        | c == ','  = commit p >>= run v
        | otherwise = run v (p + 1)
  where c   = v ! p
        len = V.length v

internal :: (Memory -> Memory) -> StateT Machine IO ()
internal f = do
  (mem, stack) <- get
  put (f mem, stack)

next :: Int -> StateT Machine IO Int
next p = do
  internal M.nex
  return $ p + 1

back :: Int -> StateT Machine IO Int
back p = do
  internal M.pre
  return $ p + 1

increment :: Int -> StateT Machine IO Int
increment p = do
  internal M.inc
  return $ p + 1

decrement :: Int -> StateT Machine IO Int
decrement p = do
  internal M.dec
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
  case M.cur mem of
    0 -> return $ p + 1
    _ -> return q

output :: Int -> StateT Machine IO Int
output p = do
  (mem, stack) <- get
  liftIO $ putChar (cur mem) >> hFlush stdout
  return $ p + 1

commit :: Int -> StateT Machine IO Int
commit p = do
  (mem, stack) <- get
  char <- liftIO getChar
  put (M.ins mem char, stack)
  return $ p + 1

