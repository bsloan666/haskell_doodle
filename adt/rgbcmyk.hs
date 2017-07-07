-- Blake G. Sloan
-- 2017 07 05

-- Algebraic Data Types (Homework)

data Colour = White | Black | Red | Green | Blue |
              RGB Int Int Int |
              CMYK Float Float Float Float deriving Show


toRGB :: Colour -> (Int, Int, Int)
-- Converts a Colour object to a tuple of red, green, and blue colours. 
-- Use the formula suggested in this link for CMYK:
-- http://www.rapidtables.com/convert/color/cmyk-to-rgb.htm
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
-- Converts the given RGB tuple into a Colour object. Return Nothing if 
-- the given numbers are outside range 0-255.
fromRGB (r, g, b) = 
    if (( r >= 0 && r < 256 ) &&
        ( g >= 0 && g < 256 ) &&
        ( b >= 0 && b < 256 ))
        then Just (RGB r g b)
        else Nothing
    
clampByte :: Int -> Int
-- Mohsen provided this neater way to do this. 
clampByte = min 255 . max 0  

indexOf :: Int -> Char -> [Char] -> Int
-- Ah recursion. And why isn't this in the standard library?
indexOf z y [] = 0
indexOf z y (x:xs) =
    if ( y == x )
        then z
        else indexOf (z+1) y xs 
     
byteToHex :: Int -> String
-- Convert an Int between 0 and 255 to the hex string equivalent
byteToHex x = 
    [a,b] 
    where a = digits !! (floor ((fromIntegral x)/16))
          b = digits !! (x - ((floor ((fromIntegral x)/16)) * 16))
          digits = "0123456789ABCDEF"

hexToInt :: String -> Int
-- Convert a hex string to its Integer equivalent
hexToInt [x,y] =
    a * 16 + b
    where a = indexOf 0 x digits
          b = indexOf 0 y digits    
          digits = "0123456789ABCDEF"
           
brighter :: Colour -> Colour
-- Makes RGB colours 10% larger. None of the RGB components can be 
-- larger than 255.
brighter c = RGB (clampByte (round ((fromIntegral r) * s))) 
                 (clampByte (round ((fromIntegral g) * s))) 
                 (clampByte (round ((fromIntegral b) * s)))
    where (r, g, b) = toRGB c
          s = 1.1  

toHexString :: Colour -> String
-- converts the colour object into a hex string, e.g., toHexString 
-- White is "#FFFFFF".
toHexString c =
    "#" ++ (byteToHex r) ++ (byteToHex g) ++ (byteToHex b) 
    where (r, g, b) = toRGB c

fromHexString :: String -> Either String Colour
-- Converts a hex string into a Colour object. Returns an appropriate 
-- error message if the input string is not well formatted.
fromHexString [] = Left "Error: Zero length hex string"
fromHexString [a,b,c,d,e,f,g] = 
    if ( ( a == '#') && (elem b digits) && (elem c digits) && (elem d digits) && (elem e digits) && (elem f digits) && (elem g digits) )
        then Right (RGB ( hexToInt [b,c] ) ( hexToInt [d,e] ) ( hexToInt [f,g] ))  
        else Left ("Error: hex colour string must contain pound sign followed by 6 hex digits")
    where digits = "0123456789ABCDEF"
fromHexString xs = Left "Error: hex colour string must contain # ++ 6 hex digits"

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
    putStrLn "rgbcmyk:  fromHexString (toHexString (brighter (RGB 10 20 30)))"
    putStrLn (show (fromHexString (toHexString c)))
    putStrLn "rgbcmyk:  fromHexString \"FlibberSplat\")"
    putStrLn (show (fromHexString "FlibberSplat"))
     
 
