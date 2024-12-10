{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Control.DeepSeq (NFData, rnf)
import Criterion.Main
import System.Random (randomRIO)

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

-- Main function with benchmarks
main :: IO ()
main = do
    randomList <- generateRandomList 1000 1 1000
    let linkedList = fromList randomList
    let linkedList2 = fromList randomList

    defaultMain
        [ bgroup "append"
            [ bench "append 1000 elements" $ nf (foldl (\acc x -> append x acc) linkedList) randomList
            ]
        , bgroup "insertFirst"
            [ bench "insertFirst 1000 elements" $ nf (foldl (\acc x -> insertFirst x acc) linkedList) randomList
            ]
        , bgroup "insertLast"
            [ bench "insertLast 1000 elements" $ nf (foldl (\acc x -> insertLast x acc) linkedList) randomList
            ]
        , bgroup "deleteFirst"
            [ bench "deleteFirst 1000 elements" $ nf (foldl (\acc _ -> deleteFirst acc) linkedList) [1..1000]
            ]
        , bgroup "deleteLast"
            [ bench "deleteLast 1000 elements" $ nf (foldl (\acc _ -> deleteLast acc) linkedList) [1..1000]
            ]
        , bgroup "reverse1"
            [ bench "reverse1" $ nf reverse1 linkedList
            ]
        , bgroup "merge"
            [ bench "merge two linked lists" $ nf (merge linkedList) linkedList2
            ]
        ]
