-- Blake G. Sloan
-- 2017 8 12 

-- Common Typeclasses (Homework #6)

-- Used the previous lesson's homework as a template.

data Tree a = Nil | Node (Tree a) a (Tree a)  deriving (Eq, Ord) 

-- Print as many spaces as your tree depth
indent :: Int -> String
indent 0 = "" 
indent i = replicate i ' ' 

-- Recursive show for trees
recshow :: Show a => Int -> Tree a -> [Char] 
recshow depth Nil = indent depth ++ "Nil"
recshow depth (Node l x r) =  concat [ indent (depth), show x, "\n", 
                                       recshow (depth + 1) l, "\n", 
                                       recshow (depth + 1) r ]

instance (Monoid a, Ord a) => Monoid (Tree a) where
  mempty = empty
  --mappend :: Tree a -> Tree a -> Tree a
  mappend Nil (Node l c r) = (Node l c r)
  mappend (Node l c r) Nil = (Node l c r)
  mappend (Node x y z) (Node l c r) = 
    if c > y
      then Node x y (Node l c r)
      else Node (Node l c r) y z
 
instance Show a => Show (Tree a) where
  show = recshow 0 

empty = Nil

-- From follow-up homework assignment
fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insert) empty

-- Solution to follow-up homework assignment
toList :: Ord t => Tree t -> [t]
toList Nil = []
toList (Node l x r) = [x] ++ toList l ++ toList r   

-- From the original lesson #5 homework
--insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = Node Nil x Nil
insert x (Node left d right) =
  if x > d
    then Node left d (insert x right)
    else Node (insert x left) d right

-- Also defined in the assignment notes
--contains :: Ord a => a -> Tree a -> Bool 
contains x Nil = False
contains x (Node l e r)
  | x == e = True
  | x < e = contains x l
  | otherwise = contains x r

main = do
    putStrLn ""
    putStrLn (show [8, 4, 1]) 
    putStrLn (show [5, 3]) 
    let tree1 = fromList [8, 4, 1]
    let tree2 = fromList [5, 3]
    putStrLn "moretypes: show tree1"
    putStrLn $ show tree1
    putStrLn "moretypes: show tree2"
    putStrLn $ show tree2
    let list2 = toList tree2
    putStrLn (show list2)
    putStrLn "Typeclasses: show tree1 ++ tree2"
    putStrLn (show (mappend (fromList [8, 4, 1])  (fromList [5,3])))
