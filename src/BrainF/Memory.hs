module BrainF.Memory where

-- | メモリ 左, 現在, 右
data Memory = Memory
  { backward :: [Char]
  , current  :: Char
  , forward  :: [Char]
  }

-- | 初期状態
initial :: Memory
initial = Memory <$> repeat <*> id <*> repeat $ toEnum 0

-- | 現在位置の値を increment
increment :: Memory -> Memory
increment = Memory <$> backward <*> (succ . current) <*> forward

-- | 現在位置の値を decrement
decrement :: Memory -> Memory
decrement = Memory <$> backward <*> (pred . current) <*> forward

-- | 現在位置を次に
step :: Memory -> Memory
step mem = Memory (c:bs) f fs where
  bs   = backward mem
  c    = current mem
  f:fs = forward mem

-- | 現在位置を前に
back :: Memory -> Memory
back mem = Memory bs b (c:fs) where
  b:bs = backward mem
  c    = current mem
  fs   = forward mem

-- | 現在位置の値を replace
replace :: Char -> Memory -> Memory
replace c (Memory bs _ fs) = Memory bs c fs
