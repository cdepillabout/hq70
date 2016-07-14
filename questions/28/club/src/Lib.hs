{-# LANGUAGE TupleSections #-}

module Lib where

import Prelude

import Data.Array ( Array, Ix, (!), listArray )
import Data.Foldable ( maximumBy )
import Data.Function ( on )
import Data.List ( subsequences )
import Data.Monoid ( Sum(..) )

type Area = Int
type Members = Int

totalAllowedMembers :: Members
totalAllowedMembers = 150

clubs :: [(Area, Members)]
clubs =
    [ (11000, 40)
    , (8000, 30)
    , (400, 24)
    , (800, 20)
    , (900, 14)
    , (1800, 16)
    , (1000, 15)
    , (7000, 40)
    , (100, 10)
    , (300, 12)
    ]

allClubCombinations :: [[(Area, Members)]]
allClubCombinations = subsequences clubs

allClubCombinationsWithLessThan150Members :: [[(Area, Members)]]
allClubCombinationsWithLessThan150Members =
    filter ((<= totalAllowedMembers) . totalMembers) allClubCombinations

totalArea :: [(Area, Members)] -> Area
totalArea = getSum . foldMap (Sum . fst)

totalMembers :: [(Area, Members)] -> Members
totalMembers = getSum . foldMap (Sum . snd)

maxArea :: [[(Area, Members)]] -> [(Area, Members)]
maxArea = maximumBy (compare `on` totalArea)

answer :: Area
answer = totalArea $ maxArea allClubCombinationsWithLessThan150Members

--------------------------------------------------------------

dynamicAnswerEmpty :: Array (Area, Members) (Maybe Area)
dynamicAnswerEmpty = listArray ((0, 0), (length clubs, totalAllowedMembers)) $ repeat Nothing

type ClubIndex = Int
type MemberIndex = Int

-- updateForClub :: ClubIndex -> (Area, Members) -> Array (Area, Members) (Maybe Area) -> Array (Array, Members) (Maybe Area)
-- updateForClub clubIndex (area, members) = go 0 
--   where
--     go :: MemberIndex -> Array (Area, Members) (Maybe Area) -> Array (Array, Members) (Maybe Area)
--     go memberIndex arr = undefined

--     positionsToUpdate :: [MemberIndex]
--     positionsToUpdate = undefined

-- updateForClub :: 

allFromRow
    :: Ix a => a -> Array (a, Members) (Maybe b) -> [(a, Maybe b)]
allFromRow a arr = fmap (\member -> (a, arr ! (a,member))) [0..totalAllowedMembers]

allNonNothingFromRow
    :: forall a b . Ix a => a -> Array (a, Members) (Maybe b) -> [(a, b)]
allNonNothingFromRow a arr = foldr f [] $ allFromRow a arr
  where
    f :: (a, Maybe b) -> [(a, b)] -> [(a, b)]
    f (_, Nothing) accum = accum
    f (a, Just b) accum = (a,b) : accum
