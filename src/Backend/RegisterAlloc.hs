module RegisterAlloc where

import Prelude as P
import Data.Map as M
import Data.Maybe

import QuadruplesData
import LivenessCheck
import LivenessCheckData
import Assembler
import RegisterAllocData

-- Gets the type of a given variable (represented by a string) by first
-- looking for its declaration in the live statements list. If found, the type
-- is extracted from the declaration statement, otherwise the type of the variable
-- is looked for in the argument list 
getTypeFromLiveness :: [LiveStmt] -> [Arg] -> String -> Type
getTypeFromLiveness liveness args s =
  if P.null found then
    fst $ fromJust $ findArg args s
  else
    fromJust $ getDeclTyp $ getLivenessStmt $ head found
      where found = P.filter (isLivenessDeclName s) liveness

-- Modifies ranges of a register (given as argument) in a RegisterRange list, by
-- allocating all variables from a given list of variable ranges 
modifyRegStateRange :: Register -> [VarRange] -> RegisterRange -> RegisterRange
modifyRegStateRange r1 xrange ((r2, range2):rgs)
  | r1 == r2 = (r1, findRanges xrange range2) : rgs
  | otherwise = (r2, range2) : modifyRegStateRange r1 xrange rgs

-- Finds all ranges in a given list of ranges that can allocate a given list of
-- VarRanges. The newly updated ranges list with allocated variables is returned
findRanges :: [VarRange] -> [Range] -> [Range]
findRanges xrange range = P.foldl findSingularRange range xrange

-- Finds an unoccupied range in a given list of ranges that could fit a given VarRange
-- and when found, updates this given range in the list by dissecting it into 3 parts: 
-- pre-alloc and post-alloc, which remain unoccupied and a new range occupied by a 
-- variable from VarRange argument. This function is called only after we made sure
-- that there exists a space for that variable
findSingularRange :: [Range] -> VarRange -> [Range]
findSingularRange ((Nothing, from1, to1):rgs) (x, from2, to2) | from1 <= from2 && to1 >= to2 =
  concat [pre, [(Just x, from2, to2)], post, rgs]
    where
      pre = [(Nothing, from1, from2 - 1) | from1 < from2]
      post = [(Nothing, to2 + 1, to1) | to1 > to2]
findSingularRange (rg:rgs) x = rg : findSingularRange rgs x

-- Modifies a given Register State data so that it mirrors the fact of allocating a given 
-- VarRanges xrange to register r and putting the variable with name x to ValMap to mark
-- the fact that this variable can be found under a scaled register r (scaled by type of variable)  
putToRegister :: [LiveStmt] -> [Arg] -> String -> Register -> [VarRange] -> RegisterState -> RegisterState
putToRegister liveness args x r xrange (RegState range vm stc) =
  RegState (modifyRegStateRange (increase r) xrange range) (modifyRegStateMap x (VReg (shrink r typ)) vm) stc
    where typ = getTypeFromLiveness liveness args x

-- For a given list of variables and their liveness, find a register in
-- the current Register state (from RegisterRange table) that could allocate all
-- of the variables from the list. If no register could allocate it, return Nothing
getFreeRegister :: [VarRange] -> RegisterState -> Maybe Register
getFreeRegister xrange (RegState range _ _) =
  if P.null res then Nothing else Just (head res)
    where
      res = P.map fst (P.filter (isRegSpace xrange) range)

-- For a given list of variables, check if all of them can be allocated
-- by a given register and its occupancy ranges (allocation definition can be found
-- in the isSingularRegSpace function)
isRegSpace :: [VarRange] -> (Register, [Range]) -> Bool
isRegSpace xrange (r, ranges) = all (isSingularRegSpace ranges) xrange

-- For a given variable and its range where it is alive (VarRange), check if some range of 
-- some register in given list of ranges could allocate it (It is free in a range 
-- that completely covers the liveness range of a given variable). If such range exists,
-- return true, otherwise return false.
isSingularRegSpace :: [Range] -> VarRange -> Bool
isSingularRegSpace [] _ = False
isSingularRegSpace ((Nothing, from1, to1):rgs) (_, from2, to2) | from1 <= from2 && to1 >= to2 = True
isSingularRegSpace (_:rgs) x = isSingularRegSpace rgs x

-- Updates a given Register State by putting the already established variable locations in 
-- assembly code (The variable is always an argument of a function). If the argument variable
-- is stored in a register, the RegisterRange and ValMap is updated in the RegisterState to
-- reflect that fact (using a putToRegister function). If the argument variable is stored
-- in memory, then we check if it is alive anywhere in the list of VarRanges. 
-- If it is and there is a free register that can hold this variable for its alive period,
-- then the Register State is modified to reflect the fact that the argument reserves
-- the register for its alive period. The ValMap is updated as well to reflect the fact that
-- the argument is stored under a memory address
-- If it is but there isn't a free register that can hold it for its duration of being alive,
-- we only update the ValMap to reflect the fact that the argument is stored under a memory address. 
-- If it isn't alive anywhere it is ignored
putCurrRanges :: [LiveStmt] -> [Arg] -> [VarRange] -> RegisterState -> (String, [AVal]) -> RegisterState
putCurrRanges liveness args vran rstate (x, [VReg r]) =
  putToRegister liveness args x r (P.filter (isVarRangeName x) vran) rstate
putCurrRanges liveness args vran rstate@(RegState range vm stc) (x, [val]) =
  case getFreeRegister found modrstate of
    Nothing -> modrstate
    Just r -> putToRegister liveness args x r found modrstate
    where
      found = P.filter (isVarRangeName x) vran -- Get all ranges of that variable
      modrstate = if P.null found then rstate else RegState range (modifyRegStateMap x val vm) stc

-- For a given list of liveness variable ranges, merge all entires that describe
-- the same variable (first element of xrange). The variable ranges were merged to a list,
-- so a final form of the result is a list of pairs of variables, and all their ranges when
-- they're alive. Grouping was done using a set
mergeSameVars :: [VarRange] -> [(String, [(Integer, Integer)])]
mergeSameVars xrange = toList $ fromListWith (++) (createGroups xrange)

-- Updates a given Register State by putting a new variable described by the pair (x, fromto).
-- If it is possible to store the variable inside a register for all of its alive period, then
-- the RegisterRange and valMap of given state is updated by putToRegister function. Otherwise,
-- if it's not possible to find a free register, the ValMap and Stack top is updated using a 
-- putToMemory function.
putNewRanges :: [LiveStmt] -> [Arg] -> RegisterState -> (String, [(Integer, Integer)]) -> RegisterState
putNewRanges liveness args rstate (x, fromto) =
  case getFreeRegister modvar rstate of
    Nothing -> putToMemory liveness args x rstate
    Just r -> putToRegister liveness args x r modvar rstate
    where modvar = P.map (mergedToVarRange x) fromto

-- Update a given Register State by putting a variable on top of the stack. ValMap is updated
-- by adding a new memory location of that variable (The memory location sits on top of the stack). 
-- The stack top is shifted by the inserted variable size to reflect that a new variable will appear 
-- there. This function is called only if there is no space in the registers for that variable. 
putToMemory :: [LiveStmt] -> [Arg] -> String -> RegisterState -> RegisterState
putToMemory liveness args x (RegState ranges vm stc) =
  RegState ranges (modifyRegStateMap x (VMem RBP Nothing (Just (-stc-size)) (Just typ)) vm) (stc + size)
    where
      typ = getTypeFromLiveness liveness args x
      size = getTypeSize typ

-- Merge a given variable and a range described as (from, to) into
-- a VarRange type (a triple of variable and it's liveness range)
mergedToVarRange :: String -> (Integer, Integer) -> VarRange
mergedToVarRange x (from, to) = (x, from, to)

-- alloc is the main endpoint of the RegisterAlloc file. Its goal is to
-- create a RegisterState for some block of code, given the current value map
-- (places where variables are stored), arguments of the function and liveness sets
-- in betweent the statements. First of all the function creates VarRanges using liveness
-- sets (VarRange describes the ranges where variables are alive), Secondly it creates
-- an initial register state. Then the register range is updated with already establised
-- variables (which are arguments saved in registers and memory). Then all variables from
-- a given block of live statements are allocated based by their liveness ranges
-- either in registers or in memory
alloc :: ValMap -> [Arg] -> [LiveStmt] -> RegisterState
alloc currvmap args liveness = shiftRegStateStack res 8
  where
    varRan = createVarRanges liveness
    inits = RegState (P.map (initialRegisterRange (length liveness)) modifiableRegisters) [] 8
    sargs = P.foldl (putCurrRanges liveness args varRan) inits currvmap
    res = P.foldl (putNewRanges liveness args) sargs (mergeSameVars (P.filter (isNotVarRangeNames (getArgNames args)) varRan))
