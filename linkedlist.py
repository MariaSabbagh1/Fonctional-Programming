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
