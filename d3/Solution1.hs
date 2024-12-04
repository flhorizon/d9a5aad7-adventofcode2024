
{-# LANGUAGE DerivingVia #-}

import Control.Applicative hiding (many)
import Data.Functor
import Control.Monad
import Data.List
import System.IO
import System.Environment
import Text.Printf
import Data.Maybe
import Data.Function
import Data.Char
import Data.Monoid
import Text.ParserCombinators.ReadP

newtype Op = Op (Int,Int)
    deriving Show

runOp :: Op -> Int
runOp (Op (l,r)) = l * r

main :: IO ()
main = do
    let defaultFile args = listToMaybe args & fromMaybe "input"
    fileName <- defaultFile <$> getArgs
    printf "Using file name: %s\n" fileName
    contents <- readFile fileName
    let 
        parser = many parseOne
        results = readP_to_S parser contents
    -- print results
    let actualResult =
            -- the best solution has the most parsing results (fst)
            -- or the least input remainder (snd)
            sortOn (length . snd) results & head & fst
    -- print actualResult
    print $ foldl (\a op -> a + runOp op) 0 actualResult

parseOne :: ReadP Op
parseOne =
    parseOne' <++ reset
  where
    reset = do
        get -- we've met an irrelevant character or an 'm' which turned out not to lead to a valid instruction
        parseOne
    parseOne' = do
        string "mul("
        lOperand <- read @Int <$> many1 (satisfy isDigit)  
        char ','
        rOperand <- read @Int <$> many1 (satisfy isDigit)  
        char ')'
        pure $ Op (lOperand,rOperand)
