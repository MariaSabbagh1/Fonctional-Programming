import time
import random
from memory_profiler import profile

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
    
    def DeleteFirst (self):
        if self.head is None:
            print("The list is empty there is nothing to delete")
        else:
            self.head = self.head.next_node
    
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
            
    @profile       
    def time_function(self, func_name, *args):
        start = time.perf_counter_ns()  # High-resolution timer
        result = getattr(self, func_name)(*args)
        end = time.perf_counter_ns()
        elapsed_ms = (end - start) / (10**6)  # Convert ns to ms
        print(f"{func_name} took {elapsed_ms:.6f} ms")
        return result            
            
    
        
        

if __name__ == "__main__":
    # Initialize list1 with 1000 random elements
    list1 = linkedlist()
    for _ in range(1000):
        list1.append(random.randint(1, 10000))  # Random integers between 1 and 1000
    print("list1:", list1.display())

    # Insert operations
    list1.time_function("InsertFirst", random.randint(1, 10000))
    #print("After InsertFirst:", list1.display())

    list1.time_function("InsertLast", random.randint(1, 10000))
    #print("After InsertLast:", list1.display())

    # Delete operations
    list1.time_function("DeleteFirst")
    #print("After DeleteFirst:", list1.display())

    list1.time_function("DeleteLast")
    #print("After DeleteLast:", list1.display())

    # Find element by index
    index = 50  
    value = list1.time_function("FindByIndex", index)
    #print(f"The element at index {index} is:", value)

    # Reverse the list
    list1.time_function("Reverse")
    #print("Reversed list:", list1.display())

    # Initialize list2 with 1000 random elements
    list2 = linkedlist()
    for _ in range(100):
        list2.append(random.randint(1, 10000))  
    #print("list2:", list2.display())

    # Merge list2 into list1
    list1.time_function("Merge", list2)
    #print("Merged List:", list1.display())
