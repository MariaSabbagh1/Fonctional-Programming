data Node a = Empty | Node a (Node a) deriving (Show)

type Linkedlist a = Node a

display :: Show a => Linkedlist a -> String
display Empty = ""
display (Node x Empty) = show x
display (Node x xs) = show x ++ " , " ++ display xs

append :: a -> Linkedlist a -> Linkedlist a
append x Empty = Node x Empty --if the list is empty
append x (Node y ys) = Node y (append x ys) --append the next node

insertFirst :: a -> Linkedlist a -> Linkedlist a
insertFirst x Empty = Node x Empty
insertFirst x (Node y ys) = Node x (Node y ys)


insertLast :: a-> Linkedlist a -> Linkedlist a  
insertLast x Empty = Node x Empty
insertLast x (Node y ys) = Node y (append x ys) 

deleteFirst :: Linkedlist a -> Linkedlist a
deleteFirst Empty = Empty
deleteFirst (Node _ xs) = xs

deleteLast :: Linkedlist a -> Linkedlist a
deleteLastt Empty = Empty 
deleteLast (Node _ Empty) = Empty --only one elt to remove
deleteLast (Node x (Node _ Empty)) = Node x Empty --only two elts to remove
deleteLast (Node x xs) = Node x (deleteLast xs)

findByIndex :: Int -> Linkedlist a -> Maybe a
findByIndex _ Empty = Nothing
findByIndex 0 (Node x _) = Just x
findByIndex n (Node _ xs)
    | n > 0     = findByIndex (n - 1) xs  -- Recur for the next node
    | otherwise = Nothing


reverse1 :: Linkedlist a -> Linkedlist a
reverse1 = rev1 Empty
  where 
     rev1 acc Empty = acc
     rev1 acc (Node x xs) = rev1 (append x acc) xs

merge :: Linkedlist a -> Linkedlist a -> Linkedlist a
merge Empty ys = ys
merge (Node x xs) ys = Node x (merge xs ys)



main :: IO ()
main = do
    let linkedList = Empty 
    let linkedList1 = append 1 linkedList 
    let linkedList2 = append 2 linkedList1
    let linkedList3 = append 3 linkedList2  
    let linkedList4 = insertFirst 0 linkedList3
    let linkedList5 = insertLast 4 linkedList4
    let linkedList6 = deleteFirst linkedList5
    let linkedList7 = deleteLast linkedList6
    let index =2
    let invalidIndex = 5
    let linkedList8 = reverse1 linkedList7
    let linkedList9 = merge linkedList8 linkedList7
    
    putStrLn $ "list : [" ++ display linkedList3 ++ "]"
    putStrLn $ "insert first : [" ++ display linkedList4 ++ "]"
    putStrLn $ "insert last :[" ++ display linkedList5 ++ "]"
    putStrLn $ "delete first : [" ++ display linkedList6 ++ "]"
    putStrLn $ "delete last : [" ++ display linkedList7 ++ "]"
    



    case findByIndex index linkedList7 of
        Just value -> putStrLn $ "Element at index " ++ show index ++ ": " ++ show value
        Nothing -> putStrLn $ "Index " ++ show index ++ " is out of range"
    
   
    case findByIndex invalidIndex linkedList7 of
        Just value -> putStrLn $ "Element at index " ++ show invalidIndex ++ ": " ++ show value
        Nothing -> putStrLn $ "Index " ++ show invalidIndex ++ " is out of range"

  
    putStrLn $ "reversed list [" ++ display linkedList8 ++ "]"
    putStrLn $ "merged list [" ++ display linkedList9 ++ "]"
