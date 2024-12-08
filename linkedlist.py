class Node :
    def __init__(self, value, next_node=None): #None because the last node doesnt point to any other node.
        self.value = value
        self.next_node = next_node
        

class linkedlist:
    def __init__(self):
        self.head = None #List is initially empty.
        
    def display(self) :
        current = self.head
        liste = [] #list to store our values      
        while current:
            liste.append(current.value)
            current = current.next_node
        return liste
    
    
    def append (self, value) : 
        new_node=Node(value)
        if self.head is None:
            self.head = new_node
        else: 
            current = self.head #we start from the head 
            while current.next_node: #we keep going until we reach the last node
                current = current.next_node 
            current.next_node = new_node #we put the new node in the end
                

    def InsertFirst(self,value):
        new_node = Node(value)
        if self.head is None:
            self.head = new_node
        else :
            new_node.next_node = self.head  
            self.head = new_node
        
    def InsertLast (self,value):
        new_node = Node (value)
        if self.head is None:
            self.head = new_node
        else: 
            current=self.head
            while current.next_node:
                current = current.next_node
            current.next_node = new_node
    
    def DeleteLast (self):
        if self.head is None:
            print("The list is empty there is nothing to delete")
        elif self.head.next_node is None: #if there is only 1 node
            self.head = None 
            return
        
        current = self.head
        while current.next_node and current.next_node.next_node:
            current = current.next_node
        current.next_node = None

    def FindByIndex (self,index):
        current = self.head
        count = 0
        while current:
            if count == index:
                return current.value
            count += 1
            current = current.next_node
        return "Index Out Of Range"
    def Reverse (self):
        previous = None
        current = self.head
        while current:
            next_node = current.next_node
            current.next_node = previous
            previous = current
            current = next_node
        self.head = previous
        
    def Merge (self, list2):
        if self.head is None:
            self.head = list2.head
        elif list2.head is None:
            return
        else:
            current = self.head
            while current.next_node:
                current = current.next_node
            current.next_node = list2.head
            
list1 = linkedlist()
list1.append(4)
list1.append(5)
list1.append(6)
print ("list : ", list1.display())
list1.InsertFirst(10)
print("insert 10 as the first element of the liste: ", list1.display())
list1.InsertLast(70)
print("insert 70 as the last element of the liste: ", list1.display())
list1.DeleteFirst()
print("delete the first element of the liste: ", list1.display())
list1.DeleteLast()
print("delete the last element of the list : " ,list1.display())
index = 2
print(f"The element at the index {index} is : " , list1.FindByIndex(index)) 
index = 6
print(list1.FindByIndex(index))
print ("The list :",list1.display())
list1.Reverse()
print("This is the reversed list : ", list1.display())
list2 = linkedlist()
list2.append(1)
list2.append(2)
print("list2 : ", list2.display())
list1.Merge(list2)
print ("Merged List: ", list1.display())
