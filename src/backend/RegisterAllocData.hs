module RegisterAllocData where

import LivenessCheckData
import Assembler as A
import SsaData as S

-- Which var occupies register, from, to
type Range = (Maybe String, Integer, Integer)

type RegisterRange = [(Register, [Range])]

type ValMap = [(String, [AVal])]

initialRegisterRange :: Int -> Register -> (Register, [Range])
initialRegisterRange len r = (r, [(Nothing, 1, fromIntegral len)])

type Arg = (S.Type, String)

findArg :: [Arg] -> String -> Maybe Arg
findArg [] _ = Nothing
findArg (arg@(typ, s1):args) s2 | s1 == s2 = Just arg
findArg (_:args) s2 = findArg args s2

getArgNames :: [Arg] -> [String]
getArgNames = map snd

data RegisterState = RegState {
  range :: RegisterRange,
  vMap :: ValMap,
  stack :: Integer
}

shiftRegStateStack :: RegisterState -> Integer -> RegisterState
shiftRegStateStack (RegState ran vmap stack) shift = RegState ran vmap (stack - shift)