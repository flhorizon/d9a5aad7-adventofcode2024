{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

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
    let 
        mkReport = map (read @Int) . words
        applyShade report i = 
            splitAt i report & \(beforeI,iTail) -> beforeI ++ drop 1 iTail 
        reduceLine (acc@(i,nSafeReports),shade) report = do
            let 
                dampenedReport = maybe report (applyShade report . fst) shade
                analyzeSafety ok@(Pending odr) (x:x':xs)
                    | not $ isMonotonic odr x x' = NonMonotonic
                    | not $ hasAcceptableDerivative x x' = UnacceptableDerivative
                    | otherwise = analyzeSafety (Pending (x `compare` x')) (x':xs)
                analyzeSafety soFar _ = soFar
                safetyReport = analyzeSafety (Pending EQ) dampenedReport
            -- printf "[L:%d] (#safe=%d) (mask[%s]) %s -> %s\n"
            --     i nSafeReports 
            --     (maybe "x" (show . fst) shade)
            --     (show dampenedReport)
            --     (show safetyReport) 
            case (safetyReport, shade) of
                (Pending _, _) ->
                    pure @IO ((succ i, succ nSafeReports),Nothing)
                (_, fromMaybe (-1,length report) -> (iShadow,shadowLimit)) ->
                    let newShade = succ iShadow
                        in if newShade < shadowLimit then
                                reduceLine (acc, Just (newShade,shadowLimit)) report
                            else
                                pure ((succ i, nSafeReports),Nothing)

    ((imax,safeAmount),_) <- foldM (\a -> reduceLine a . mkReport) ((1 :: Int, 0 :: Int),Nothing) inputLines
    printf "There are %d safe reports over %d\n" safeAmount imax
