-- data Tree a = Node {rootLabel :: a, subForest :: Forest a}
-- type Forest a = [Tree a]
import Data.Tree

-- treeFold :: (a -> [b] -> b) ->  Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold  f) (subForest t))

-- depth :: Tree a -> Int
depth = treeFold (\x xs -> 1 + (foldr max 0 xs)) -- It is easy to prove correctness!

-- treeMap :: (a -> b) -> (Tree a) -> (Tree b)
treeMap f = treeFold (\x xs -> Node (f x) xs)

doubleTree  = treeMap (*2)
sumTree     = treeFold (\x xs -> x + sum xs)
fringeTree  = treeFold (\x xs -> x : concat xs) -- Or treeFold (\x xs -> [x] ++ (concat xs))
displayTree = drawTree . (treeMap show)

-- t :: Tree Int
t = Node 3
      [Node 8
        [Node 7 [],
         Node 2 
          [Node 9 [],
           Node 1 []]],
         Node 5
          [Node 6
            [Node 11 []]]]

main = do putStrLn $ "Elements of the Tree are " ++ (show $ fringeTree t)
          putStrLn $ "Doubled tree is \n" ++  (displayTree $ doubleTree t)
          putStrLn $ "Depth of the tree is " ++ (show $ depth t) 
