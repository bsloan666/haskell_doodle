-- Blake G. Sloan
-- 2017 07 18 

-- More Types (Homework)

-- Much of the code is from the notes.

data Tree a = Nil | Node (Tree a) a (Tree a)  deriving (Eq, Ord) 

indent :: Int -> String
indent 0 = "" 
indent i = replicate i ' ' 


recshow :: Show a => Int -> Tree a -> [Char] 
recshow depth Nil = concat [(indent depth), "Nil"]
recshow depth (Node l x r) =  concat [ (indent (depth)), show x, "\n", 
                                        recshow (depth + 1) l, "\n", 
                                        recshow (depth + 1) r ]

instance Show a => Show (Tree a) where
  show (Node l x r) = (recshow 0 (Node l x r)) 
  show a = show a

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
     
 
