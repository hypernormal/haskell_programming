myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Nothing -> []
                  Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
               Nothing -> Leaf
               Just (a, b, c) -> Node (unfold f a) b (unfold f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\b -> if b == 0 then Nothing else Just (b-1, b, b-1)) n
