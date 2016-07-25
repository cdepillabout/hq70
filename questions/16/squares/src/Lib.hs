{-# LANGUAGE TupleSections #-}

module Lib where

import Prelude

import Control.Monad ( guard )
import Data.Foldable ( toList )
import Data.List ( nub, nubBy )
import Data.Sequence ( Seq, index, replicateM )

type StringLength = Int
type SideLength = Int
type Area = Int

allAnswers :: [(Int, Float, Float)]
allAnswers = do
    squareSideLength <- [1 .. 125]
    let edges = [1 .. squareSideLength - 1]
        otherSquareAreas =
            rectangleAreaFromSquareSideLength squareSideLength <$> edges
        repped = replicateM 2 otherSquareAreas
        allTwoRectangles = unsafeSeqToTuple <$> repped
    (rect1, rect2) <- allTwoRectangles
    guard (rect1 + rect2 == squareSideLength * squareSideLength)
    pure
        ( 1
        , fromIntegral rect2 / fromIntegral rect1
        , fromIntegral squareSideLength * fromIntegral squareSideLength / fromIntegral rect1
        )

-- uniqueAnswers = nubBy uniqueCheck allAnswers
-- uniqueAnswers = nubBy uniqueCheck2 allAnswers
uniqueAnswers :: [(Int, Float, Float)]
uniqueAnswers = nub allAnswers

run :: IO ()
run = print $ length uniqueAnswers

uniqueCheck :: (Eq a, Eq b) => (a, b, b) -> (a, b, b) -> Bool
uniqueCheck (x, a1, b1) (y, b2, a2) =
    (x == y) &&
        (
            ( (a1 == a2) && (b1 == b2) )
        ||
            ( (a1 == b2) && (b1 == a2) )
        )

uniqueCheck2 :: Eq a => (a, a, a) -> (a, a, a) -> Bool
uniqueCheck2 (a, b, c) (x, y, z) =
        ((a == x) || (a == y) || (a == z))
    &&
        ((b == x) || (b == y) || (b == z))
    &&
        ((c == x) || (c == y) || (c == z))

unsafeSeqToTuple :: Seq a -> (a, a)
unsafeSeqToTuple seq = (index seq 0, index seq 1)

rectangleAreaFromSquareSideLength :: SideLength -> SideLength -> Area
rectangleAreaFromSquareSideLength squareSideLength rectangleSideLength =
    rectangleSideLength * (2 * squareSideLength - rectangleSideLength)
