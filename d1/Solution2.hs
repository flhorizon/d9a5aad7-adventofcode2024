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
    let defaultFile args = listToMaybe args & fromMaybe "example"
    fileName <- defaultFile <$> E.getArgs
    printf "File name: %s\n" fileName
    inputLines <- fmap lines . readFile $ fileName
    printf "File has %d lines\n" (length inputLines)
    let reduceLine l (ls,rs) = 
            let [a,b] = words l
                upsert exists = fmap succ exists <|> Just @Int 1
             in (read a : ls
                , M.alter upsert (read b) rs)
        (values, registry) = 
            foldr reduceLine mempty inputLines

        score = foldl (\ac val ->
                M.lookup val registry &
                maybe ac ((+ac) . (*val))
            ) 0 values
    printf "The similarity score is: %d\n" score
