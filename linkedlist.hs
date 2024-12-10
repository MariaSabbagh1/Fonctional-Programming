{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Control.DeepSeq (NFData, rnf, deepseq)
import Criterion.Main
import System.Random (randomRIO)
import GHC.Stats (getRTSStats, RTSStats(allocated_bytes), getRTSStatsEnabled)
import System.Mem (performGC)
import System.Clock (Clock(Monotonic), getTime, diffTimeSpec, toNanoSecs)

-- Define the LinkedList data structure
data Node a = Empty | Node a (Node a) deriving (Show, Generic)
type Linkedlist a = Node a

-- Derive NFData for benchmarking
instance NFData a => NFData (Node a) where
    rnf Empty = ()
    rnf (Node x next) = rnf x `seq` rnf next

-- Append function
append :: a -> Linkedlist a -> Linkedlist a
append x Empty = Node x Empty
append x (Node y ys) = Node y (append x ys)

-- Insert at the beginning
insertFirst :: a -> Linkedlist a -> Linkedlist a
insertFirst x Empty = Node x Empty
insertFirst x (Node y ys) = Node x (Node y ys)

-- Insert at the end
insertLast :: a -> Linkedlist a -> Linkedlist a
insertLast x Empty = Node x Empty
insertLast x (Node y ys) = Node y (append x ys)

-- Delete the first element
deleteFirst :: Linkedlist a -> Linkedlist a
deleteFirst Empty = Empty
deleteFirst (Node _ xs) = xs

-- Delete the last element
deleteLast :: Linkedlist a -> Linkedlist a
deleteLast Empty = Empty
deleteLast (Node _ Empty) = Empty
deleteLast (Node x (Node _ Empty)) = Node x Empty
deleteLast (Node x xs) = Node x (deleteLast xs)

-- Reverse the list
reverse1 :: Linkedlist a -> Linkedlist a
reverse1 = rev1 Empty
  where
    rev1 acc Empty = acc
    rev1 acc (Node x xs) = rev1 (append x acc) xs

-- Merge two linked lists
merge :: Linkedlist a -> Linkedlist a -> Linkedlist a
merge Empty ys = ys
merge (Node x xs) ys = Node x (merge xs ys)

-- Create a LinkedList from a list
fromList :: [a] -> Linkedlist a
fromList [] = Empty
fromList (x:xs) = Node x (fromList xs)

-- Generate a list of random integers
generateRandomList :: Int -> Int -> Int -> IO [Int]
generateRandomList n low high = sequence (replicate n (randomRIO (low, high)))

-- Measure both execution time and memory usage for a function
measureTimeAndMemory :: NFData a => IO a -> IO ()
measureTimeAndMemory action = do
    statsEnabled <- getRTSStatsEnabled
    if not statsEnabled
        then putStrLn "RTS Stats are not enabled. Run GHCi with +RTS -T."
        else do
            performGC -- Force garbage collection for accurate measurement
            
            -- Start time and memory
            startStats <- getRTSStats
            let startMem = allocated_bytes startStats
            startTime <- getTime Monotonic
            
            -- Perform the action
            result <- action
            result `deepseq` return () -- Force evaluation
            
            -- End time and memory
            endStats <- getRTSStats
            let endMem = allocated_bytes endStats
            endTime <- getTime Monotonic
            
            -- Calculate time and memory usage
            let elapsedTime = fromIntegral (toNanoSecs (diffTimeSpec startTime endTime)) / (10^6) -- Convert to milliseconds
            let memoryUsed = endMem - startMem
            
            -- Print results
            putStrLn $ "Execution time: " ++ show elapsedTime ++ " ms"
            putStrLn $ "Memory used: " ++ show memoryUsed ++ " bytes"

-- Main function with memory profiling
runMemoryProfiling :: IO ()
runMemoryProfiling = do
    randomList <- generateRandomList 1000 1 1000
    let linkedList = fromList randomList
    let linkedList2 = fromList randomList

    putStrLn "Performance for operations:"
    measureTimeAndMemory $ return $ foldl (\acc x -> append x acc) Empty randomList
    putStrLn "Append profiling completed."

    measureTimeAndMemory $ return $ foldl (\acc x -> insertFirst x acc) Empty randomList
    putStrLn "InsertFirst profiling completed."

    measureTimeAndMemory $ return $ foldl (\acc x -> insertLast x acc) Empty randomList
    putStrLn "InsertLast profiling completed."

    measureTimeAndMemory $ return $ foldl (\acc _ -> deleteFirst acc) linkedList [1..1000]
    putStrLn "DeleteFirst profiling completed."

    measureTimeAndMemory $ return $ foldl (\acc _ -> deleteLast acc) linkedList [1..1000]
    putStrLn "DeleteLast profiling completed."

    measureTimeAndMemory $ return $ reverse1 linkedList
    putStrLn "Reverse profiling completed."

    measureTimeAndMemory $ return $ merge linkedList linkedList2
    putStrLn "Merge profiling completed."
