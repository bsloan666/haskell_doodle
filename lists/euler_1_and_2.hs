-- Blake G. Sloan
-- 2017 6 20



-- EulerProject #1

mltpl35 :: Int -> Bool
mltpl35 x = ((x `mod` 5) == 0) || ((x `mod` 3) == 0 )
 
euler1 :: Int -> Int
-- sum all multiples of 3 and 5 between 0 and n 
euler1 0 = 0
euler1 n = 
    sum (filter mltpl35 [0..n-1])

-- EulerProject #2

takeWhile' :: (a -> Bool) -> [a] -> [a] 
-- borrowed from my solutions to exercises 
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x then [x] ++ takeWhile' f xs else []

fibo :: Int -> Int
-- give me the  nth fibonaci term
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

fibs :: Int -> [Int]
--the first n fibonaci terms
fibs 0 = []
fibs n = map fibo [0..n-1] 

fibsLessThan :: Int -> [Int]
-- get me the list of fibonaci terms less than n
fibsLessThan 0 = []
fibsLessThan n = takeWhile' ( < n) (map fibo [0..])  

even' :: Int -> Bool
even' x = if (x `mod` 2) == 1 then False else True    

main = do
    putStrLn ""
    putStrLn "euler1"
    putStrLn "(sum of multiples of 3 or 5 less than 1000)"
    putStrLn (show( euler1 1000))
    putStrLn ""
    putStrLn "fibsLessThan 1000"
    putStrLn (show (fibsLessThan 1000))
    putStrLn ""
    putStrLn "euler2"
    putStrLn "(sum of even fibs less than 4000000)"
    putStrLn (show (sum (filter even' (fibsLessThan 4000000))))

