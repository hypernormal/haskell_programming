data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show, Eq)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
     then print "yup okay!"
     else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = concat [[a], preorder left, preorder right]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = concat [inorder left, [a], inorder right]

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = concat [postorder left, postorder right, [a]]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b bt = foldr f b (preorder bt)

mapTree' :: (a -> b) -> BinaryTree a -> [b]
mapTree' f bt = foldTree ((:) . f) [] bt
