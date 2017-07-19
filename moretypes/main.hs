-- Blake G. Sloan
-- 2017 07 18 

-- More Types (Homework)

-- Much of the code is from the notes.

data Tree a = Nil | Node (Tree a) a (Tree a)  deriving (Eq, Ord, Read)

indent :: Int -> String
indent 0 = "" 
indent i = replicate i ' ' 

depth :: Tree a -> Int
depth Nil = 0
depth (Node l _ r) = 1 + max (depth l) (depth r)

instance Show a => Show (Tree a) where
  show Nil = "Nil"
  show (Node l x r) = do
    let d = depth l
    concat [ (indent (d-1)), show x, "\n", 
             (indent d), show l, "\n", 
             (indent d), show r ]

empty = Nil

insert x Nil = Node Nil x Nil
insert x (Node left d right) =
  if x > d
    then Node left d (insert x right)
    else Node (insert x left) d right

contains x Nil = False
contains x (Node l e r)
  | x == e = True
  | x < e = contains x l
  | otherwise = contains x r

main = do
    putStrLn ""
    let tree = foldr insert empty [8, 4, 1, 5, 3]
    putStrLn "moretypes: show tree"
    putStrLn (show( tree))
     
 
