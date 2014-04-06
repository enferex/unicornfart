import Data.Bits(xor)
import Data.Digest.Pure.MD5(md5)
import Data.ByteString.Lazy.Internal(ByteString, packChars)
import System.Environment(getArgs)

type Hash = String
data Chain = Chain String Hash deriving (Show)

-- Reduction1: Given a hashed string, return the first three chars 
reduce1 :: Hash -> String
reduce1 h = take 5 h

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

-- Given a plaintext, mutate it and return a new plaintext
nextPlaintext :: String -> String
nextPlaintext p = map succ p -- TODO: Implement better routine

-- Generates a list of 'n' chains (e.g., a rainbow table)
-- Initially 'c' is [] and represents the [Chain] of generate chains
buildTable :: Int -> String -> [Chain] -> [Chain]
buildTable 0 _ c = c
buildTable n p c = buildTable (n-1) (nextPlaintext p) $ (buildChain p) : c

main = do 
    args <- getArgs
    mapM print $ buildTable 10 (args !! 0) []
