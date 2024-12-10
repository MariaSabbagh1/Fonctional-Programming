data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Show, Eq)

-- Insert 
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

-- Search 
search :: Ord a => a -> BinaryTree a -> Bool
search _ Empty = False
search x (Node y left right)
    | x == y    = True
    | x < y     = search x left
    | otherwise = search x right

-- Find the minimum value 
findMin :: BinaryTree a -> a
findMin (Node x Empty _) = x
findMin (Node _ left _)  = findMin left
findMin Empty = error "Empty tree has no minimum"

-- Remove 
remove :: Ord a => a -> BinaryTree a -> BinaryTree a
remove _ Empty = Empty
remove x (Node y left right)
    | x < y     = Node y (remove x left) right
    | x > y     = Node y left (remove x right)
    | otherwise = case (left, right) of
                    (Empty, Empty) -> Empty
                    (Empty, _)     -> right
                    (_, Empty)     -> left
                    _              -> let minValue = findMin right
                                       in Node minValue left (remove minValue right)

-- In-order traversal
inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

-- Pre-order traversal
preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node x left right) = [x] ++ preOrder left ++ preOrder right

-- Post-order traversal
postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node x left right) = postOrder left ++ postOrder right ++ [x]

-- Main
main :: IO ()
main = do
    let tree = foldr insert Empty [50, 30, 70, 20, 40, 60, 80]
    putStrLn $ "In-order traversal: " ++ show (inOrder tree)
    putStrLn $ "Pre-order traversal: " ++ show (preOrder tree)
    putStrLn $ "Post-order traversal: " ++ show (postOrder tree)
    putStrLn $ "Search 40: " ++ show (search 40 tree)
    putStrLn $ "Search 100: " ++ show (search 100 tree)
    let treeAfterRemoval = remove 50 tree
    putStrLn $ "In-order traversal after removing 50: " ++ show (inOrder treeAfterRemoval)
