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



main = do
    putStrLn ""
    putStrLn "euler1 1000"
    putStrLn (show( euler1 1000))
