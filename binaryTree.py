import random
import time  # for time complexity
import sys  # for space complexity
import tracemalloc  # for auxiliary memory

# Calculate the memory of a node
def node_memory(node):
    if not node:
        return 0
    return sys.getsizeof(node) + node_memory(node.left) + node_memory(node.right)

# Node class
class Node:
    def __init__(self, key):
        self.key = key
        self.left = None
        self.right = None

# Binary Tree class
class BinaryTree:

    # Constructor
    def __init__(self):
        self.root = None

    # Insert method
    def insert(self, key):
        if not self.root:
            self.root = Node(key)
        else:
            self._insert(self.root, key)

    def _insert(self, current, key):
        if key < current.key:
            if current.left:
                self._insert(current.left, key)
            else:
                current.left = Node(key)
        else:
            if current.right:
                self._insert(current.right, key)
            else:
                current.right = Node(key)

    # Search method
    def search(self, key):
        return self._search(self.root, key)

    def _search(self, current, key):
        if not current:
            return False
        if current.key == key:
            return True
        elif key < current.key:
            return self._search(current.left, key)
        else:
            return self._search(current.right, key)

    # Remove method
    def remove(self, key):
        self.root = self._remove(self.root, key)

    def _remove(self, current, key):
        if not current:
            return current
        if key < current.key:
            current.left = self._remove(current.left, key)
        elif key > current.key:
            current.right = self._remove(current.right, key)
        else:
            if not current.left:
                return current.right
            elif not current.right:
                return current.left
            min_larger_node = self._find_min(current.right)
            current.key = min_larger_node.key
            current.right = self._remove(current.right, min_larger_node.key)
        return current

    def _find_min(self, current):
        while current.left:
            current = current.left
        return current

    # In-order traversal
    def inorder_traversal(self):
        result = []
        self._inorder(self.root, result)
        return result

    def _inorder(self, current, result):
        if current:
            self._inorder(current.left, result)
            result.append(current.key)
            self._inorder(current.right, result)

    # Pre-order traversal
    def preorder_traversal(self):
        result = []
        self._preorder(self.root, result)
        return result

    def _preorder(self, current, result):
        if current:
            result.append(current.key)
            self._preorder(current.left, result)
            self._preorder(current.right, result)

    # Post-order traversal
    def postorder_traversal(self):
        result = []
        self._postorder(self.root, result)
        return result

    def _postorder(self, current, result):
        if current:
            self._postorder(current.left, result)
            self._postorder(current.right, result)
            result.append(current.key)

# Main execution
if __name__ == "__main__":
    bt = BinaryTree()
    n = 10000  # Number of elements
    print(f"\nNumber of elements: {n}")

    # Time for inserts
    start = time.time()
    for i in range(n - 1):
        bt.insert(random.randint(1, 999999))
    # Insert the n-th element to be removed later
    bt.insert(872)
    end = time.time()
    print(f"\nTime taken to insert {n} elements: {(end - start):.5f} seconds")

    # Measure tree memory
    tree_memory = node_memory(bt.root)
    print(f"Memory used by the tree: {tree_memory} bytes")

    # Time and auxiliary memory for in-order traversal
    start = time.time()
    tracemalloc.start()
    bt.inorder_traversal()
    end = time.time()
    current, peak = tracemalloc.get_traced_memory()
    print(f"\nAuxiliary memory for in-order traversal: {current} bytes")
    tracemalloc.stop()
    print(f"Time taken for in-order traversal of {n} elements: {(end - start):.5f} seconds")

    # Time and auxiliary memory for pre-order traversal
    start = time.time()
    tracemalloc.start()
    bt.preorder_traversal()
    end = time.time()
    current, peak = tracemalloc.get_traced_memory()
    print(f"\nAuxiliary memory for in-order traversal: {current} bytes")
    tracemalloc.stop()
    print(f"Time taken for pre-order traversal of {n} elements: {(end - start):.5f} seconds")

    # Time and auxiliary memory for post-order traversal
    start = time.time()
    tracemalloc.start()
    bt.postorder_traversal()
    end = time.time()
    current, peak = tracemalloc.get_traced_memory()
    print(f"\nAuxiliary memory for in-order traversal: {current} bytes")
    tracemalloc.stop()
    print(f"Time taken for post-order traversal of {n} elements: {(end - start):.5f} seconds")

    # Time to search an element (worst case scenario)
    start = time.time()
    print(f"\nSearch 88901: {bt.search(88901)}")
    end = time.time()
    print(f"Time taken to search for 88901: {(end - start):.5f} seconds")

    # Time to remove an element (worst case scenario)
    start = time.time()
    bt.remove(872)
    end = time.time()
    print(f"\nTime taken to remove 872: {(end - start):.5f} seconds")
