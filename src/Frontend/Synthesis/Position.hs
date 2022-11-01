module Position where

import AbsLatte

data Pos = BNFC BNFC'Position
         | Default

instance Show Pos where
  show (BNFC (Just (x, y))) = concat [
    "at line ", show x,
    ", position ", show y
    ]
  show (BNFC Nothing) = "at unspecified location (BNFC internal error)"
  show Default = "in default function"

toPos :: BNFC'Position -> Pos
toPos = BNFC