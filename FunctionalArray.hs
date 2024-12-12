import GHC.Generics (Generic)
import Control.DeepSeq (NFData, rnf, deepseq)
import Criterion.Main
import System.Random (randomRIO)
import GHC.Stats (getRTSStats, RTSStats(allocated_bytes), getRTSStatsEnabled)
import System.Mem (performGC)
import System.Clock (Clock(Monotonic), getTime, diffTimeSpec, toNanoSecs)

data FunctionalArray a = FunctionalArray [a] deriving (Show, Generic)


instance NFData a => NFData (FunctionalArray a) where
    rnf (FunctionalArray elements) = rnf elements


get :: Int -> FunctionalArray a -> Maybe a
get index (FunctionalArray elements)
  | index >= 0 && index < length elements = Just (elements !! index)
  | otherwise = Nothing


update :: Int -> a -> FunctionalArray a -> FunctionalArray a
update index value (FunctionalArray elements)
  | index >= 0 && index < length elements =
      let (before, _:after) = splitAt index elements
      in FunctionalArray (before ++ [value] ++ after)
  | otherwise = error "Index out of bounds"


append :: a -> FunctionalArray a -> FunctionalArray a
append value (FunctionalArray elements) = FunctionalArray (elements ++ [value])


prepend :: a -> FunctionalArray a -> FunctionalArray a
prepend value (FunctionalArray elements) = FunctionalArray (value : elements)


lengthArray :: FunctionalArray a -> Int
lengthArray (FunctionalArray elements) = length elements


slice :: Int -> Int -> FunctionalArray a -> FunctionalArray a
slice start end (FunctionalArray elements) = FunctionalArray (take (end - start) (drop start elements))


mapArray :: (a -> b) -> FunctionalArray a -> FunctionalArray b
mapArray f (FunctionalArray elements) = FunctionalArray (map f elements)


filterArray :: (a -> Bool) -> FunctionalArray a -> FunctionalArray a
filterArray p (FunctionalArray elements) = FunctionalArray (filter p elements)


foldArray :: (b -> a -> b) -> b -> FunctionalArray a -> b
foldArray f acc (FunctionalArray elements) = foldl f acc elements


insertFirst :: a -> FunctionalArray a -> FunctionalArray a
insertFirst = prepend


insertLast :: a -> FunctionalArray a -> FunctionalArray a
insertLast = append


deleteFirst :: FunctionalArray a -> FunctionalArray a
deleteFirst (FunctionalArray []) = FunctionalArray []
deleteFirst (FunctionalArray (_:xs)) = FunctionalArray xs


deleteLast :: FunctionalArray a -> FunctionalArray a
deleteLast (FunctionalArray []) = FunctionalArray []
deleteLast (FunctionalArray xs) = FunctionalArray (init xs)


reverse1 :: FunctionalArray a -> FunctionalArray a
reverse1 (FunctionalArray elements) = FunctionalArray (reverse elements)


merge :: FunctionalArray a -> FunctionalArray a -> FunctionalArray a
merge (FunctionalArray xs) (FunctionalArray ys) = FunctionalArray (xs ++ ys)


findByIndex :: Int -> FunctionalArray a -> Maybe a
findByIndex = get


fromList :: [a] -> FunctionalArray a
fromList = FunctionalArray


generateRandomList :: Int -> Int -> Int -> IO [Int]
generateRandomList n low high = sequence (replicate n (randomRIO (low, high)))


measureTimeAndMemory :: NFData a => IO a -> IO ()
measureTimeAndMemory action = do
    statsEnabled <- getRTSStatsEnabled
    if not statsEnabled
        then putStrLn "RTS Stats are not enabled. Run GHCi with +RTS -T."
        else do
            performGC
            
            -- Start time and memory
            startStats <- getRTSStats
            let startMem = allocated_bytes startStats
            startTime <- getTime Monotonic
            
            -- Perform the action
            result <- action
            result `deepseq` return ()
            
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




main :: IO ()
main = runMemoryProfiling

runMemoryProfiling :: IO ()
runMemoryProfiling = do
    randomList <- generateRandomList 10000 1 10000
    let functionalArray = fromList randomList
    let functionalArray2 = fromList randomList

    putStrLn "Performance for operations:"
    measureTimeAndMemory $ return $ foldArray (\acc x -> append x acc) (FunctionalArray []) randomList
    putStrLn "Append profiling completed."

    measureTimeAndMemory $ return $ foldArray (\acc x -> insertFirst x acc) (FunctionalArray []) randomList
    putStrLn "InsertFirst profiling completed."

    measureTimeAndMemory $ return $ foldArray (\acc x -> insertLast x acc) (FunctionalArray []) randomList
    putStrLn "InsertLast profiling completed."

    measureTimeAndMemory $ return $ foldArray (\acc _ -> deleteFirst acc) functionalArray [1..10000]
    putStrLn "DeleteFirst profiling completed."

    measureTimeAndMemory $ return $ foldArray (\acc _ -> deleteLast acc) functionalArray [1..10000]
    putStrLn "DeleteLast profiling completed."

    measureTimeAndMemory $ return $ reverse1 functionalArray
    putStrLn "Reverse profiling completed."

    measureTimeAndMemory $ return $ merge functionalArray functionalArray2
    putStrLn "Merge profiling completed."

    measureTimeAndMemory $ return $ findByIndex 5000 functionalArray
    putStrLn "FindByIndex profiling completed."

    measureTimeAndMemory $ return $ get 5000 functionalArray
    putStrLn "Get profiling completed."

    measureTimeAndMemory $ return $ update 5000 9999 functionalArray
    putStrLn "Update profiling completed."

    measureTimeAndMemory $ return $ prepend 0 functionalArray
    putStrLn "Prepend profiling completed."

    measureTimeAndMemory $ return $ lengthArray functionalArray
    putStrLn "Length profiling completed."

    measureTimeAndMemory $ return $ slice 1000 2000 functionalArray
    putStrLn "Slice profiling completed."

    measureTimeAndMemory $ return $ mapArray (+1) functionalArray
    putStrLn "MapArray profiling completed."

    measureTimeAndMemory $ return $ filterArray even functionalArray
    putStrLn "FilterArray profiling completed."

    measureTimeAndMemory $ return $ foldArray (+) 0 functionalArray
    putStrLn "FoldArray profiling completed."

generateRandomList :: Int -> Int -> Int -> IO [Int]
generateRandomList n low high = sequence (replicate n (randomRIO (low, high)))

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
            result `deepseq` return ()
            
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