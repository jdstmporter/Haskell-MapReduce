module Main where

import System.Environment (getArgs)
import System.IO
import System.Random (getStdRandom,randomR)

dictionary :: String
dictionary = "/usr/share/dict/words"

main::IO()
main = do
        args <- getArgs
        (file,text) <- case length args of
                3 -> do
                        let n    = read $ head args
                            m    = read $ args!!1
                            file = args!!2
                        words <- getWords m dictionary
                        text <- makeText words n m 
                        return (file,text)
                _ -> error "Usage: wordgen [nwords] [vocabsize] [file]"
        putLines file text
                



makeText :: [String] -> Int -> Int -> IO [String] 
makeText words n m = do 
        is <- getRand 0 (m-1) n
        -- putStrLn $ "Got "++(show $ length is)++" numbers"
        let ws = map (\i -> words!!i) is
        -- putStrLn $ "Got "++(show $ length ws)++" words"
        return ws
        
getRand:: Int -> Int -> Int -> IO [Int]
getRand min max n
        | n <=0 = return []
        | otherwise = do
                xs <- getRand min max (n-1)
                x <- getStdRandom (randomR (min,max))
                return (x:xs)           

getWords :: Int -> String -> IO [String]
getWords n dict = do
        h <- openFile dict ReadMode
        text <- hGetContents h
        return (take n $ lines text)      
            
            
putLines :: FilePath -> [String] -> IO ()
putLines file text = do
        h <- openFile file WriteMode
        hPutStr h $ unlines text
        hClose h
        return () 