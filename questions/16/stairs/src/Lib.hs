module Lib where

import Prelude

type NumOfStairs = Int

type StepDist = Int
type AStepDist = StepDist
type BStepDist = StepDist

type CurrentStair = Int
type ACurrentStair = CurrentStair
type BCurrentStair = CurrentStair

type MeetStair = CurrentStair

stepDistances :: [StepDist]
stepDistances = [1,2,3,4]

allCombinations :: NumOfStairs -> [Maybe MeetStair]
allCombinations numOfStairs =
    doMeet numOfStairs 0 numOfStairs

doMeet :: NumOfStairs -> ACurrentStair -> BCurrentStair -> [Maybe MeetStair]
doMeet numOfStairs aCurrentStair bCurrentStair
    | aCurrentStair > bCurrentStair = pure Nothing
    | aCurrentStair == bCurrentStair = pure $ Just aCurrentStair
    | otherwise = do
        aNextStepDist <- stepDistances
        bNextStepDist <- stepDistances
        let aNextStair = aCurrentStair + aNextStepDist
            bNextStair = bCurrentStair - bNextStepDist
        doMeet numOfStairs aNextStair bNextStair
