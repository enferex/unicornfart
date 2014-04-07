-- unicornfart.hs: A haskell based rainbow table generator
--
-- Originally Copyright (C) 2014 Matt Davis (enferex)
--
-- This is free software, fork it and have at it!
--
import Data.Char(chr, ord)
import Data.Digest.Pure.MD5(md5)
import Data.ByteString.Lazy.Internal(ByteString, packChars)
import System.Environment(getArgs)

type Hash = String
data Chain = Chain String Hash

instance Show Chain where
    show (Chain p h) = "<" ++ p ++ ", " ++ h ++ ">"

-- Reduction1: Given a hashed string, return the first three chars 
-- TODO: Add more reductions
reduce1 :: Hash -> String
reduce1 h = take 16 h

-- List of reduction functions (one per link in the chain)
reductions :: [Hash -> String]
reductions  = [reduce1]

-- Apply reduction functions to given plain text
reduce :: Hash -> [Hash -> String] -> String
reduce p [] = p
reduce p (r:rs) = reduce (r (hash' p)) rs

-- Hash a plaintext string
hash' :: String -> Hash
hash' = show . md5 . packChars

-- Create a chain where the startpoint is the plaintext and endpoint, the
-- reduced hash value starting from that plaintext.
buildChain :: String -> Chain
buildChain p = (Chain p $ reduce (hash' p) reductions)

-- Add with carry (carry starting with value of c)
addCarry :: Int -> [Int] -> [Int]
addCarry c (x:[]) = (x + c) `mod` 127 : []
addCarry c (x:xs) = ((x + c) `mod` 127) : (addCarry adder xs)
    where adder = fromEnum (x + c == 127)

-- Given a plaintext, mutate it and return a new plaintext
-- TODO: Implement better routine
--
-- 1) convert [Char] to [Int]
-- 2) Increment and carry
nextPlaintext :: String -> String
nextPlaintext p = reverse $ map chr $ vals
    where vals = addCarry 1 $ reverse $ map ord p

-- Generates a list of 'n' chains (e.g., a rainbow table)
-- Initially 'c' is [] and represents the [Chain] of generate chains
buildTable :: Int -> String -> [Chain] -> [Chain]
buildTable 0 _ c = c
buildTable n p c = buildTable (n-1) (nextPlaintext p) $ (buildChain p) : c

main = do 
    args <- getArgs
    mapM_ print $ buildTable 5 (args !! 0) []
