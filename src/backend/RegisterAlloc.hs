module RegisterAlloc where

import Prelude as P
import Data.Map as M
import Data.Maybe

import SsaData
import LivenessCheck
import LivenessCheckData
import Assembler
import RegisterAllocData

getTypeFromLiveness :: [LiveStmt] -> [Arg] -> String -> Type
getTypeFromLiveness liveness args s =
  if P.null found then
    fst $ fromJust $ findArg args s
  else
    fromJust $ getDeclTyp $ getLivenessStmt $ head found
      where found = P.filter (isLivenessDeclName s) liveness

modifyRegStateRange :: Register -> [VarRange] -> RegisterRange -> RegisterRange
modifyRegStateRange r1 xrange ((r2, range2):rgs) 
  | r1 == r2 = (r1, findRanges xrange range2) : rgs
  | otherwise = (r2, range2) : modifyRegStateRange r1 xrange rgs

findRanges :: [VarRange] -> [Range] -> [Range]
findRanges xrange range = P.foldl findSingularRange range xrange

findSingularRange :: [Range] -> VarRange -> [Range]
findSingularRange ((Nothing, from1, to1):rgs) (x, from2, to2) | from1 <= from2 && to1 >= to2 =
  concat [pre, [(Just x, from2, to2)], post, rgs]
    where
      pre = if from1 < from2 then [(Nothing, from1, from2 - 1)] else []
      post = if to1 > to2 then [(Nothing, to2 + 1, to1)] else []
findSingularRange (rg:rgs) x = rg : findSingularRange rgs x

putToRegister :: [LiveStmt] -> [Arg] -> String -> Register -> [VarRange] -> RegisterState -> RegisterState
putToRegister liveness args x r xrange (RegState range vm stc) =
  RegState (modifyRegStateRange (increase r) xrange range) (modifyRegStateMap x (VReg (shrink r typ)) vm) stc
    where typ = getTypeFromLiveness liveness args x

getFreeRegister :: [VarRange] -> RegisterState -> Maybe Register
getFreeRegister xrange (RegState range _ _) =
  if P.null res then Nothing else Just (head res)
    where
      res = P.map fst (P.filter (isRegSpace xrange) range)

isRegSpace :: [VarRange] -> (Register, [Range]) -> Bool
isRegSpace xrange (r, ranges) = all (isSingularRegSpace ranges) xrange

isSingularRegSpace :: [Range] -> VarRange -> Bool
isSingularRegSpace [] _ = False
isSingularRegSpace ((Nothing, from1, to1):rgs) (_, from2, to2) | from1 <= from2 && to1 >= to2 = True
isSingularRegSpace (_:rgs) x = isSingularRegSpace rgs x

putCurrRanges :: [LiveStmt] -> [Arg] -> [VarRange] -> RegisterState -> (String, [AVal]) -> RegisterState
putCurrRanges liveness args vran rstate (x, [VReg r]) =
  putToRegister liveness args x r (P.filter (isVarRangeName x) vran) rstate
putCurrRanges liveness args vran rstate@(RegState range vm stc) (x, [val]) =
  case getFreeRegister found modrstate of
    Nothing -> modrstate
    Just r -> putToRegister liveness args x r found modrstate
    where
      found = P.filter (isVarRangeName x) vran
      modrstate = if P.null found then rstate else RegState range (modifyRegStateMap x val vm) stc

modifyRegStateMap :: String -> AVal -> ValMap -> ValMap
modifyRegStateMap x v [] = [(x, [v])]
modifyRegStateMap x1 v ((x2, vs):vms)
  | x1 == x2 = (x2, v : vs) : vms
  | otherwise = (x2, vs) : modifyRegStateMap x1 v vms

mergeSameVars :: [VarRange] -> [(String, [(Integer, Integer)])]
mergeSameVars xrange = toList $ fromListWith (++) (createGroups xrange)

putNewRanges :: [LiveStmt] -> [Arg] -> RegisterState -> (String, [(Integer, Integer)]) -> RegisterState
putNewRanges liveness args rstate (x, fromto) =
  case getFreeRegister modvar rstate of
    Nothing -> fillIn liveness args x rstate
    Just r -> putToRegister liveness args x r modvar rstate
    where modvar = P.map (mergedToVarRange x) fromto

fillIn :: [LiveStmt] -> [Arg] -> String -> RegisterState -> RegisterState
fillIn liveness args x (RegState ranges vm stc) =
  RegState ranges (modifyRegStateMap x ((VMem RBP Nothing (Just (-stc-size))) (Just typ)) vm) (stc + size)
    where
      typ = getTypeFromLiveness liveness args x
      size = getTypeSize typ

mergedToVarRange :: String -> (Integer, Integer) -> VarRange
mergedToVarRange x (from, to) = (x, from, to)

alloc :: ValMap -> [Arg] -> [LiveStmt] -> RegisterState
alloc currvmap args liveness = 
  shiftRegStateStack res 8
    where
      varRan = createVarRanges liveness
      inits = RegState (P.map (initialRegisterRange (length liveness)) modifiableRegisters) [] 8 
      sargs = P.foldl (putCurrRanges liveness args varRan) inits currvmap
      res = P.foldl (putNewRanges liveness args) sargs (mergeSameVars (P.filter (isNotVarRangeNames (getArgNames args)) varRan))
