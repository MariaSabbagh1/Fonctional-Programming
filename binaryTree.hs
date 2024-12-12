import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import System.Mem (performGC)
import GHC.Stats (getRTSStatsEnabled, getRTSStats, GCDetails(..), RTSStats(..))
import Text.Printf (printf)
import Control.DeepSeq (NFData (rnf), deepseq)
import System.Random (randomRIO)

-- Binary Tree
data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Show, Eq)

instance NFData a => NFData (BinaryTree a) where
  rnf Empty = ()
  rnf (Node x left right) = rnf x `seq` rnf left `seq` rnf right


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

-- Traversals (in-order, pre-order, post-order)
inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node x left right) = [x] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node x left right) = postOrder left ++ postOrder right ++ [x]

-- timing and memory
measureTime :: IO a -> IO Double
measureTime action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    return $ realToFrac (diffUTCTime end start) * 1e9 -- Convert to nanoseconds

measureMemory :: IO a -> IO Integer
measureMemory action = do
    performGC
    statsBefore <- getRTSStats
    _ <- action
    performGC
    statsAfter <- getRTSStats
    let liveBytesBefore = fromIntegral $ gcdetails_live_bytes (gc statsBefore)
        liveBytesAfter = fromIntegral $ gcdetails_live_bytes (gc statsAfter)
    return $ liveBytesAfter - liveBytesBefore


printPretty :: String -> Double -> String -> IO ()
printPretty task duration unit = printf "%s: %.0f %s\n" task duration unit

-- function to generate lists of n random elements
generateRandomNumbers :: Int -> IO [Int]
generateRandomNumbers n = mapM (\_ -> randomRIO (1, 1000000)) [1..n] 

-- Main
main :: IO ()
main = do
    -- Define n !!!!!!!
    let n = 10000000
    elements <- generateRandomNumbers n  
    putStrLn $ "\nNumber of elements: " ++ show n

    -- Insert 
    insertTime <- measureTime $ do
        let tree = foldr insert Empty elements
        tree `deepseq` return tree 
    printPretty "\nTime taken to insert elements" insertTime "nanoseconds"

    -- Create the tree
    let tree = foldr insert Empty elements

    -- Measure memory used by the tree
    treeMemory <- measureMemory $ tree `deepseq` return tree
    printPretty "Memory used by the tree" (fromIntegral treeMemory) "bytes"

    -- In-order traversal
    inOrderTime <- measureTime $ do
        let result = inOrder tree
        result `deepseq` return result
    inOrderMemory <- measureMemory $ let result = inOrder tree in result `deepseq` return result
    printPretty "\nTime taken for in-order traversal" inOrderTime "nanoseconds"
    printPretty "Auxiliary memory for in-order traversal" (fromIntegral inOrderMemory) "bytes"

    -- Pre-order traversal
    preOrderTime <- measureTime $ do
        let result = preOrder tree
        result `deepseq` return result
    preOrderMemory <- measureMemory $ let result = preOrder tree in result `deepseq` return result
    printPretty "\nTime taken for pre-order traversal" preOrderTime "nanoseconds"
    printPretty "Auxiliary memory for pre-order traversal" (fromIntegral preOrderMemory) "bytes"

    -- Post-order traversal
    postOrderTime <- measureTime $ do
        let result = postOrder tree
        result `deepseq` return result
    postOrderMemory <- measureMemory $ let result = postOrder tree in result `deepseq` return result
    printPretty "\nTime taken for post-order traversal" postOrderTime "nanoseconds"
    printPretty "Auxiliary memory for post-order traversal" (fromIntegral postOrderMemory) "bytes"

    -- Search for an element
    searchTime <- measureTime $ do
        let result = search (n + 1) tree
        result `deepseq` return result
    printPretty "\nTime taken to search for an element" searchTime "nanoseconds"

    -- Remove an element
    removeTime <- measureTime $ do
        let treeAfterRemoval = remove (n `div` 2) tree
        treeAfterRemoval `deepseq` return treeAfterRemoval
    printPretty "\nTime taken to remove an element" removeTime "nanoseconds"
