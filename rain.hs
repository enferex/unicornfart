import Data.Bits(xor)
import System.Environment

type Hash = Int
type Chain = (String, Hash)


md5 :: String -> Hash
md5 s = 0xdeadbeef `xor` (read s)

reduce :: Hash -> String
reduce hash = take 3 $ show hash

buildChain :: String -> Chain
buildChain plain = (plain, md5 $ reduce $ md5 plain)

buildTable :: String -> [Chain]
buildTable plain = buildChain plain : []

main = do 
    args <- getArgs
    mapM putStrLn [ p ++ "-->" ++ show h | (p, h) <- buildTable $ args !! 0]
