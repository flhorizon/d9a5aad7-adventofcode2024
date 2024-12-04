{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import System.IO
import qualified System.Environment as E
import Control.Monad
import Data.List
import Data.Bifunctor
import Text.Printf
import Data.Maybe
import Data.Function

data SafetyStatus =
    Pending Ordering
    | NonMonotonic
    | UnacceptableDerivative
    deriving (Show)

isMonotonic :: Ordering -> Int -> Int -> Bool
isMonotonic soFar l r =
    soFar == EQ || soFar == (l `compare` r)

hasAcceptableDerivative :: Int -> Int -> Bool
hasAcceptableDerivative l r =
    let dLevel = abs (l - r)
     in dLevel >= lowerBound && dLevel <= higherBound
  where
    lowerBound = 1
    higherBound = 3

isOk :: SafetyStatus -> Bool
isOk (Pending _) = True
isOk _ = False

main :: IO ()
main = do
    let defaultFile args = listToMaybe args & fromMaybe "input"
    fileName <- defaultFile <$> E.getArgs
    printf "Using file name: %s\n" fileName
    inputLines <- fmap lines . readFile $ fileName
    let reduceLine (i,nSafeReports) l = do
            let report = map (read @Int) $ words l 
                analyzeSafety ok@(Pending odr) (x:x':xs)
                    | not $ isMonotonic odr x x' = NonMonotonic
                    | not $ hasAcceptableDerivative x x' = UnacceptableDerivative
                    | otherwise = analyzeSafety (Pending (x `compare` x')) (x':xs)
                analyzeSafety soFar _ = soFar
                safetyReport = analyzeSafety (Pending EQ) report
            printf "[L:%d] (#safe=%d) %s -> %s\n" i nSafeReports (show report) (show safetyReport) 
            pure @IO (succ i, nSafeReports & if isOk safetyReport then succ else id)

    (imax,safeAmount) <- foldM reduceLine (1 :: Int, 0 :: Int) inputLines
    printf "There are %d safe reports over %d\n" safeAmount imax
