
{-# LANGUAGE TupleSections,LambdaCase #-}

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
data DoGate = DoGate
  deriving (Show)
data DoNotGate = DoNotGate
  deriving (Show)
data Token = DoTok DoGate | DoNotTok DoNotGate | OpTok Op
  deriving (Show)

runOp :: Op -> Int
runOp (Op (l,r)) = l * r

main :: IO ()
main = do
    let defaultFile args = listToMaybe args & fromMaybe "input"
    fileName <- defaultFile <$> getArgs
    printf "Using file name: %s\n" fileName
    contents <- readFile fileName
    let 
        parser = parseToken (showString "")
        results = readP_to_S (many parser) contents
    -- putStrLn "Broad results:"
    -- print results
    let _bestResult@(tokensWithInterstitialJunk,_remainder) =
            -- the best solution has the most parsing results (fst)
            -- or the least input remainder (snd)
            sortOn (length . snd) results & head
        tokens = map snd tokensWithInterstitialJunk
    -- putStrLn "Best(?) result:"
    -- print _bestResult
    -- putStrLn "VS contents:"
    -- putStrLn contents
        applyDosAndDonts acc'@(shouldDo,acc) =
            \case
                DoTok _ -> (True,acc)
                DoNotTok _ -> (False,acc)
                OpTok op -> if shouldDo then acc' $> op:acc else acc'
        curatedOps = snd $ foldl applyDosAndDonts (True,[]) tokens
        ranOpsResult = foldl (\a op -> a + runOp op) 0 curatedOps
    putStrLn "Ran Ops result is:"
    print ranOpsResult

type Junk' = ShowS

-- | Accumulate Junk for introspection
type Junk = String

parseToken :: Junk' -> ReadP (Junk, Token)
parseToken j =
    fmap (j [],) parseOne <++ reset
  where
    reset = do
        c <- get
        parseToken (j . (c:))
    parseOne = choice [DoTok <$> parseDo, DoNotTok <$> parseDont, OpTok <$> parseOp]
    parseDo = do
        string "do()"
        pure $ DoGate
    parseDont = do
        string "don't()"
        pure $ DoNotGate
    parseOp = do
        string "mul("
        lOperand <- read @Int <$> many1 (satisfy isDigit)  
        char ','
        rOperand <- read @Int <$> many1 (satisfy isDigit)  
        char ')'
        pure $ Op (lOperand,rOperand)
