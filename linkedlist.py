import time
import random
import gc
import sys

class Node:
    def __init__(self, value, next_node=None):
        self.value = value
        self.next_node = next_node

class LinkedList:
    def __init__(self):
        self.head = None
        self.tail = None  

    def display(self):
        current = self.head
        lst = []
        while current:
            lst.append(current.value)
            current = current.next_node
        return lst

    def append(self, value):
        new_node = Node(value)
        if self.head is None:
            self.head = new_node
            self.tail = new_node  
        else:
            self.tail.next_node = new_node
            self.tail = new_node  

    def InsertFirst(self, value):
        new_node = Node(value)
        new_node.next_node = self.head
        self.head = new_node
        if self.tail is None:  
            self.tail = new_node

    def InsertLast(self, value):
        self.append(value)

    def DeleteFirst(self):
        if self.head is not None:
            self.head = self.head.next_node
            if self.head is None:  
                self.tail = None

    def DeleteLast(self):
        if self.head is None:
            return
        if self.head.next_node is None:
            self.head = None
            self.tail = None
            return
        current = self.head
        while current.next_node and current.next_node.next_node:
            current = current.next_node
        current.next_node = None
        self.tail = current

    def FindByIndex(self, index):
        current = self.head
        count = 0
        while current:
            if count == index:
                return current.value
            count += 1
            current = current.next_node
        return None

    def Reverse(self):
        previous = None
        current = self.head
        while current:
            next_node = current.next_node
            current.next_node = previous
            previous = current
            current = next_node
        self.head = previous
        self.tail = self.head
        while self.tail and self.tail.next_node:
            self.tail = self.tail.next_node

    def Merge(self, list2):
        if self.head is None:
            self.head = list2.head
            self.tail = list2.tail
        elif list2.head is not None:
            self.tail.next_node = list2.head
            self.tail = list2.tail

    def time_and_memory_function(self, func_name, *args):
        gc.collect()  # Clean up garbage collection before measuring memory
        mem_before = self._get_size()  # Memory before operation
        start_time = time.perf_counter_ns()

        getattr(self, func_name)(*args)  # Perform the operation

        end_time = time.perf_counter_ns()
        gc.collect()
        mem_after = self._get_size()  # Memory after operation

        elapsed_ms = (end_time - start_time) / (10**6)  # Convert ns to ms
        memory_used = abs(mem_after - mem_before)  # Use absolute 

        print(f"Operation: {func_name}")
        print(f"Execution time: {elapsed_ms:.3f} ms")
        print(f"Memory change: {memory_used} bytes\n")

    def _get_size(self):
        """Iteratively calculates the size of the linked list in bytes."""
        total_size = 0
        current = self.head
        while current:
            total_size += sys.getsizeof(current)
            current = current.next_node
        return total_size

if __name__ == "__main__":
    list1 = LinkedList()

    # Profiling the append operation
    print("Performance for operations:")
    gc.collect()  # Clean up garbage collection before measuring memory

    start_time = time.perf_counter_ns()

    
    for _ in range(10000):
        list1.append(random.randint(1, 10000))

    end_time = time.perf_counter_ns()

    elapsed_ms = (end_time - start_time) / (10**6)  # Convert ns to ms

    print(f"Operation: append (10000 elements)")
    print(f"Execution time: {elapsed_ms:.3f} ms\n")

    # Profiling other operations
    list1.time_and_memory_function("InsertFirst", random.randint(1, 10000))
    list1.time_and_memory_function("InsertLast", random.randint(1, 10000))
    list1.time_and_memory_function("DeleteFirst")
    list1.time_and_memory_function("DeleteLast")
    list1.time_and_memory_function("FindByIndex", 50)

    list2 = LinkedList()
    for _ in range(100):  
        list2.append(random.randint(1, 10000))
    list1.time_and_memory_function("Merge", list2)
    list1.time_and_memory_function("Reverse")
