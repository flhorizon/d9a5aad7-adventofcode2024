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
import qualified Data.IntMap.Strict as M

main :: IO ()
main = do
    let defaultFile args = listToMaybe args & fromMaybe "input"
    fileName <- defaultFile <$> E.getArgs
    printf "File name: %s\n" fileName
    inputLines <- fmap lines . readFile $ fileName
    printf "File has %d lines\n" (length inputLines)
    let reduceLine l (ls,rs) = 
            let [a,b] = words l
                upsert exists = fmap succ exists <|> Just 1
             in (M.alter upsert (read a) ls
                , M.alter upsert (read b) rs)
        unfoldColumn =
            M.foldrWithKey (\n times acc -> replicate times n ++ acc) []
        sortedPairs = 
            uncurry zip .
            bimap unfoldColumn unfoldColumn $
            foldr reduceLine mempty inputLines

    printf "Got %d sorted pairs\n" (length sortedPairs)
    let distance = sum . map (abs . uncurry (-)) $ sortedPairs
    --(_,distance) <- foldM (\(i,sigma) (l, r) -> do
    --        let dist = abs (l - r) :: Int
    --        printf "[L:%d] %d `dist` %d is %d\n" i l r dist
    --        pure (succ i, sigma + dist)
    --    ) (1 :: Int, 0 :: Int) sortedPairs
    putStrLn $ "Distance sum is: " ++ show distance
