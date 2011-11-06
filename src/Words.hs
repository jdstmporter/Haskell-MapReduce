module Words where

import System.Environment (getArgs)
import System.IO
import Test.QuickCheck
import Text.Printf
import MRCode (mapReduce,countWords)

makeWord :: Gen String
makeWord = do
        n <- choose(2,10)::Gen Int
        word n
                where
                word n
                        | n==0 = return ""
                        | otherwise = do
                                c <- choose('A','Z')
                                w <- word (n-1)
                                return $ c:w

makeWordList :: [String] -> Int -> Int -> Gen [String] 
makeWordList words n m 
        | n == 0 = return []
        | otherwise = do 
                i <- choose (0,m-1)
                rem <- makeWordList words (n-1) m
                return $ (words!!i):rem

                   

makeDictionary :: Int -> Gen [String]
makeDictionary n
        | n == 0 = return []
        | otherwise = do
                w <- makeWord
                d <- makeDictionary (n-1)
                return $ w:d
 
        
makeWords :: (Int,Int) -> Gen [String]
makeWords (n,m) = do
        dict <- makeDictionary m
        makeWordList dict n m


dictionary :: String
dictionary = "/usr/share/dict/words"

prop_Equal nm = forAll (makeWords nm) $ \words -> countWords (mapReduce 16 words) == fst nm
  where types = nm::(Int,Int)

main::IO()
main  = quickCheck prop_Equal
                



