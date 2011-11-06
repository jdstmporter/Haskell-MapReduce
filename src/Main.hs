-- | Classic word count MapReduce algorithm written with the monad
--
--   Takes as argument:
--
--   * A file of word-based text
--
--   * (Optional) the number of mappers to use in the first stage
--     (defaults to 16)
module Main where


import System.IO
import System.Environment (getArgs)
import MRCode

showNice :: [(String,Int)] -> IO()
showNice [] = return ()
showNice (x:xs) = do
        putStrLn $ fst x ++ " occurs "++ show ( snd x) ++ " times"
        showNice xs 

main::IO()
main = do
        args <- getArgs
        out <- case length args of 
                0 -> error "Usage: wordcount [filename] ([num mappers])"
                _ -> do
                        let nMap = case length args of
                                1 -> 16
                                _ -> read $ args!!1
                        state <- getLines (head args)                                
                        let res = mapReduce nMap state
                        return res
        showNice out
        print (countWords out)



-- put data

putLines :: FilePath -> [String] -> IO ()
putLines file text = do
        h <- openFile file WriteMode
        hPutStr h $ unlines text
        hClose h
        return () 

-- get input

getLines :: FilePath -> IO [String]
getLines file = do
        h <- openFile file ReadMode
        text <- hGetContents h
        let l = (lines text)
        return l
        





         


 
                      