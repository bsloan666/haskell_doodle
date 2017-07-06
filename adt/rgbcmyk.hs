-- Blake G. Sloan
-- 2017 07 05

-- Algebraic Data Types (Homework)

data Colour = White | Black | Red | Green | Blue |
              RGB Int Int Int |
              CMYK Float Float Float Float deriving Show

toRGB :: Colour -> (Int, Int, Int)
toRGB White = (255,255,255) 
toRGB Black = (0,0,0) 
toRGB Red = (255,0,0) 
toRGB Green = (0,255,0) 
toRGB Blue = (0,0,255)
toRGB (RGB r g b) = ( r, g, b)
toRGB (CMYK c m y k) = (    round ( 255.0 * (1.0-c) * (1.0-k)),
                            round ( 255.0 * (1.0-m) * (1.0-k)),
                            round ( 255.0 * (1.0-y) * (1.0-k)))


fromRGB :: (Int, Int, Int) -> Maybe Colour
fromRGB (r, g, b) = 
    if (( r >= 0 && r < 256 ) &&
        ( g >= 0 && g < 256 ) &&
        ( b >= 0 && b < 256 ))
        then Just (RGB r g b)
        else Nothing
    
clampByte :: Int -> Int 
clampByte x = 
   if( x >= 0 && x < 256)
        then x
        else
            if( x < 0)
                then 0
                else 255 

byteToHex :: Int -> String
byteToHex x = 
    [a,b] 
    where a = digits !! (floor ((fromIntegral x)/16))
          b = digits !! (x - ((floor ((fromIntegral x)/16)) * 16))
          digits = "0123456789ABCDEF"
           
brighter :: Colour -> Colour
brighter c = RGB (clampByte (round ((fromIntegral r) * s))) 
                 (clampByte (round ((fromIntegral g) * s))) 
                 (clampByte (round ((fromIntegral b) * s)))
    where (r, g, b) = toRGB c
          s = 1.1  

toHexString :: Colour -> String
toHexString c =
    "#" ++ (byteToHex r) ++ (byteToHex g) ++ (byteToHex b) 
    where (r, g, b) = toRGB c

main = do
    putStrLn ""
    putStrLn "rgbcmyk: toRGB Red"
    putStrLn (show( toRGB Red))
    putStrLn "rgbcmyk: toRGB (RGB 10 20 30)"
    putStrLn (show( toRGB (RGB 10 20 30)))
    putStrLn "rgbcmyk: toRGB (CMYK 0.5 0.2 0.1 0.4)"
    putStrLn (show( toRGB (CMYK 0.5 0.2 0.1 0.4 )))
    putStrLn "rgbcmyk: fromRGB (10, 20, 30)"
    let c = fromRGB (10,20,30)
    putStrLn (show c) 
    putStrLn "rgbcmyk: fromRGB (10, 20, 300)"
    let c = fromRGB (10,20,300)
    putStrLn (show c)
    let c = brighter (RGB 10 20 30)
    putStrLn "rgbcmyk: brighter (RGB 10 20 30)"
    putStrLn (show c)
    putStrLn "rgbcmyk:  toHexString (brighter (RGB 10 20 30))"
    putStrLn (toHexString c) 
     
 
