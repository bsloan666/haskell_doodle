import Prelude
import System.Environment

-- Blake G. Sloan
-- 2017 06 20

null' :: [a] -> Bool 
-- return True if the given list is empty, False otherwise
null' [] = True
null' xs = False

elem' :: (Eq a) => a -> [a] -> Bool 
-- return True if the given element is in list, False otherwise
-- cribbed from "Learn You a Haskell...". Essentially written 
elem' _ [] = False
elem' a (x:xs) 
    | a == x  = True 
    | otherwise = a `elem'` xs
        
-- Suggest an alternative implementation for elem
elem'' :: (Eq a) => a -> [a] -> Bool 
elem'' _ [] = False
elem'' a (x:xs) = 
    if a == x then True else (elem'' a xs) 

sum' :: [Int] -> Int
-- summation of the elements of a list of integers
sum' [] = 0
sum' (x:xs) = x + sum' xs
    

--filter :: (a -> Bool) -> [a] -> [a] 
-- keep the elements that match the criterion, drop the rest

--splitAt :: Int -> [a] -> ([a], [a]) 
-- split the list into two smaller lists at the given index
-- Hint: use take and drop
-- all :: (a -> Bool) -> [a] -> Bool 
-- return True if all the elements of the list match the given criterion, False otherwise
-- Hint: use map and and

-- takeWhile :: (a -> Bool) -> [a] -> [a] 
-- takes from the list while the criterion is True
-- example: takeWhile (<5) [1, 1, 4, 3, 5, 6, 1, 10] returns [1, 1, 4, 3]

main = do
    let x1 = []
    let x2 = ["fish", "dog", "palooka"]
    let x3 = [2,43,90,8,6]
 
    putStrLn "null' []"
    putStrLn (show( null' x1))
    putStrLn "null' [\"fish\", \"dog\", \"palooka\"]"
    putStrLn (show( null' x2))
 
    putStrLn "elem' \"turd\" []"
    putStrLn (show( elem' "turd" x1))
    putStrLn "elem' \"turd\" [\"fish\", \"dog\", \"palooka\"]"
    putStrLn (show( elem' "turd" x2)) 
    putStrLn "elem' \"dog\" [\"fish\", \"dog\", \"palooka\"]"
    putStrLn (show( elem' "dog" x2))
 
    putStrLn "elem'' \"turd\" []"
    putStrLn (show( elem'' "turd" x1))
    putStrLn "elem'' \"turd\" [\"fish\", \"dog\", \"palooka\"]"
    putStrLn (show( elem'' "turd" x2)) 
    putStrLn "elem'' \"dog\" [\"fish\", \"dog\", \"palooka\"]"
    putStrLn (show( elem'' "dog" x2))
 
    putStrLn "sum' []"
    putStrLn (show( sum' x1))
    putStrLn "sum'  [2,43,90,8,6]"
    putStrLn (show( sum' x3))
