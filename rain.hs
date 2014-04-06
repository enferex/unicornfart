import Data.Bits(xor)
import Data.Digest.Pure.MD5(md5)
import Data.ByteString.Lazy.Internal(ByteString, packChars)
import System.Environment(getArgs)

type Hash = String
data Chain = Chain String Hash deriving (Show)

hash' :: String -> String
hash' = show . md5 . packChars

reduce :: Hash -> String
reduce h = take 3 $ show h

buildChain :: String -> Chain
buildChain plain = (Chain plain (hash' $ reduce $ hash' plain))

buildTable :: String -> [Chain]
buildTable plain = buildChain plain : []

main = do 
    args <- getArgs
    mapM print $ buildTable $ args !! 0
