module Position where

import AbsLatte

{- Position is used as a boxing for bnfc position, as some positions 
 - in errors may relate to default functions or classes -}

data Pos = BNFC BNFC'Position -- Standard BNFC position
         | Default -- Default position used in case of code generated by compiler
         deriving (Eq, Ord, Read)

instance Show Pos where
  show (BNFC (Just (x, y))) = concat [
    "at line ", show x,
    ", position ", show y
    ]
  show (BNFC Nothing) = "at unspecified location (BNFC internal error)"
  show Default = "in default function"

toPos :: BNFC'Position -> Pos
toPos = BNFC