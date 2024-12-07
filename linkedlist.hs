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

    putStrLn $ "[" ++ display linkedList3 ++ "]"
    putStrLn $ "[" ++ display linkedList4 ++ "]"
    putStrLn $ "[" ++ display linkedList5 ++ "]"
    putStrLn $ "[" ++ display linkedList6 ++ "]"
    putStrLn $ "[" ++ display linkedList7 ++ "]"

