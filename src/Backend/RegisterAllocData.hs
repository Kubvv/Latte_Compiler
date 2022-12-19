module RegisterAllocData where

import LivenessCheckData
import Assembler as A
import QuadruplesData as Q

-- Range is a triple that stores an information about occupancy of some register
-- in some scope, described by two Integers being lines of code. The first argument
-- is Nothing when the register is free in a given range, and Just x where x is a
-- variable name when register is occupied 
type Range = (Maybe String, Integer, Integer)

-- For a given integer and a range (of some register), check if
-- the range is ocuppied and the given integer falls in the range
isJustAndInRange :: Integer -> Range -> Bool
isJustAndInRange i (Just _, from, to) | from <= i && i <= to = True
isJustAndInRange _ _ = False

-- For a given integer and a range (of some register), check if
-- the range is unocuppied and the given integer falls in the range
isNothingAndInRange :: Integer -> Range -> Bool
isNothingAndInRange i (Nothing, from, to) | from <= i && i <= to = True
isNothingAndInRange _ _ = False

-- Getter
getArrRangeVar :: [Range] -> Maybe String
getArrRangeVar [(v, _, _)] = v

-- RegisterRange stores a list of all register and their ranges. Since every
-- register can have many different spans during the execution of code, each
-- register has a list of many possible ranges
type RegisterRange = [(Register, [Range])]

-- Create an initial registerRange by assigning no variable for a whole block of code
-- of length len to a register r
initialRegisterRange :: Int -> Register -> (Register, [Range])
initialRegisterRange len r = (r, [(Nothing, 1, fromIntegral len)])

-- For a given Integer that represents code line number and a register with its ranges,
-- Get a range of that register that is occupied by some variable at the code line number i
filterJustInRanges :: Integer -> (Register, [Range]) -> [Range]
filterJustInRanges i (_,rs) = filter (isJustAndInRange i) rs

-- For a given integer that represents code line number and a register with its ranges,
-- Check if this register has a range when it is unoccupied and the code line falls between
-- this range
anyNothingInRanges :: Integer -> (Register, [Range]) -> Bool
anyNothingInRanges i (_,rs) = any (isNothingAndInRange i) rs

-- Holds a mapping from a variable to assembly values, which basically
-- tells how can a given variable be described in the x86_64 code
type ValMap = [(String, [AVal])]

-- For a given variable and its mapping to assembly value, check
-- if the amount of representations are equal to the given number
isValueCount :: Int -> (String, [AVal]) -> Bool
isValueCount count (_,av) = length av == count

-- Get first register value in a given list of assembly values
getFirstRegister :: [AVal] -> Maybe AVal
getFirstRegister [] = Nothing
getFirstRegister (r@VReg {}:avs) = Just r
getFirstRegister (_:avs) = getFirstRegister avs

-- Get first memory value in a given list of assembly values
getFirstMemory :: [AVal] -> Maybe AVal
getFirstMemory [] = Nothing
getFirstMemory (r@VMem {}:avs) = Just r
getFirstMemory (_:avs) = getFirstMemory avs

-- For a given string describing a variable name and a ValMap,
-- modify the mapping of that variable by adding a new assembly value where
-- the variable is stored. If the entry wasn't found (a variable wasn't stored
-- anywhere), add a new entry to ValMap with that variable and its assembly value
modifyRegStateMap :: String -> AVal -> ValMap -> ValMap
modifyRegStateMap x v [] = [(x, [v])]
modifyRegStateMap x1 v ((x2, vs):vms)
  | x1 == x2 = (x2, v : vs) : vms
  | otherwise = (x2, vs) : modifyRegStateMap x1 v vms

type Arg = (Q.Type, String)

-- Find argument in the list of arguments by its name
findArg :: [Arg] -> String -> Maybe Arg
findArg [] _ = Nothing
findArg (arg@(typ, s1):args) s2 | s1 == s2 = Just arg
findArg (_:args) s2 = findArg args s2

getArgNames :: [Arg] -> [String]
getArgNames = map snd

data RegisterState = RegState {
  range :: RegisterRange, -- Holds all registers and what occupies them at every range
  vMap :: ValMap, -- Holds the mapping from a variable to all assembly location where that var is stored
  stack :: Integer -- Holds the amount of stack space required for putting variables 
                   -- on the stack (due to lack of space in registers)
}

-- Setters
putvm :: ValMap -> RegisterState -> RegisterState
putvm vm (RegState range _ stc) = RegState range vm stc

shiftRegStateStack :: RegisterState -> Integer -> RegisterState
shiftRegStateStack (RegState ran vmap stack) shift = RegState ran vmap (stack - shift)