module RegisterAllocData where

import LivenessCheckData
import Assembler as A
import SsaData as S

-- Which var occupies register, from, to
type Range = (Maybe String, Integer, Integer)

isJustAndInRange :: Integer -> Range -> Bool
isJustAndInRange i (Just _, from, to) | from <= i && i <= to = True
isJustAndInRange _ _ = False

isNothingAndInRange :: Integer -> Range -> Bool
isNothingAndInRange i (Nothing, from, to) | from <= i && i <= to = True
isNothingAndInRange _ _ = False

getArrRangeRegister :: [Range] -> Maybe String
getArrRangeRegister [(r, _, _)] = r

type RegisterRange = [(Register, [Range])]

initialRegisterRange :: Int -> Register -> (Register, [Range])
initialRegisterRange len r = (r, [(Nothing, 1, fromIntegral len)])

filterJustInRanges :: Integer -> (Register, [Range]) -> [Range]
filterJustInRanges i (_,rs) = filter (isJustAndInRange i) rs

anyNothingInRanges :: Integer -> (Register, [Range]) -> Bool
anyNothingInRanges i (_,rs) = any (isNothingAndInRange i) rs

type ValMap = [(String, [AVal])]

isValueCount :: Int -> (String, [AVal]) -> Bool
isValueCount count (_,av) = length av == count

getFirstRegister :: [AVal] -> Maybe AVal
getFirstRegister [] = Nothing
getFirstRegister (r@(VReg {}):avs) = Just r
getFirstRegister (_:avs) = getFirstRegister avs

getFirstMemory :: [AVal] -> Maybe AVal
getFirstMemory [] = Nothing
getFirstMemory (r@(VMem {}):avs) = Just r
getFirstMemory (_:avs) = getFirstMemory avs

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

putvm :: ValMap -> RegisterState -> RegisterState
putvm vm (RegState range _ stc) = RegState range vm stc

shiftRegStateStack :: RegisterState -> Integer -> RegisterState
shiftRegStateStack (RegState ran vmap stack) shift = RegState ran vmap (stack - shift)