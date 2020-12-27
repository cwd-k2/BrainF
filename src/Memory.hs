module Memory
  ( Memory
  , initial
  , inc
  , dec
  , ins
  , nex
  , pre
  , cur
  ) where

-- | メモリ 左, 現在, 右
data Memory = Memory { back    :: [Char]
                     , current :: Char
                     , forward :: [Char] }

-- | 初期状態
initial :: Memory
initial = Memory <$> repeat <*> id <*> repeat $ toEnum 0

-- | 現在位置の値を increment
inc :: Memory -> Memory
inc = Memory <$> back <*> (succ . current) <*> forward

-- | 現在位置の値を decrement
dec :: Memory -> Memory
dec = Memory <$> back <*> (pred . current) <*> forward

-- | 現在位置に値を代入
ins :: Char -> Memory -> Memory
ins val mem = Memory (back mem) val (forward mem)

-- | 現在位置を次に
nex :: Memory -> Memory
nex mem = Memory (c:bs) f fs
  where
    bs   = back mem
    c    = current mem
    f:fs = forward mem

-- | 現在位置を前に
pre :: Memory -> Memory
pre mem = Memory bs b (c:fs)
  where
    b:bs = back mem
    c    = current mem
    fs   = forward mem

-- | 現在位置の値を返す
cur :: Memory -> Char
cur = current

