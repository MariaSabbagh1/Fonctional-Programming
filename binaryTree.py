#Node
class Node:
    def __init__(self, key):
        self.key = key
        self.left = None
        self.right = None


#Binary Tree
class BinaryTree:
    
    #Construct
    def __init__(self):
        self.root = None

    #Insert method
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

    #Search
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
        
    #Remove
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
    
    #In-order traversal
    def inorder_traversal(self):
        result = []
        self._inorder(self.root, result)
        return result

    def _inorder(self, current, result):
        if current:
            self._inorder(current.left, result)
            result.append(current.key)
            self._inorder(current.right, result)

    #Pre-order traversal
    def preorder_traversal(self):
        result = []
        self._preorder(self.root, result)
        return result

    def _preorder(self, current, result):
        if current:
            result.append(current.key)
            self._preorder(current.left, result)
            self._preorder(current.right, result)

    #Post-order traversal
    def postorder_traversal(self):
        result = []
        self._postorder(self.root, result)
        return result

    def _postorder(self, current, result):
        if current:
            self._postorder(current.left, result)
            self._postorder(current.right, result)
            result.append(current.key)

# Example usage
if __name__ == "__main__":
    bt = BinaryTree()
    bt.insert(50)
    bt.insert(30)
    bt.insert(70)
    bt.insert(20)
    bt.insert(40)
    bt.insert(60)
    bt.insert(80)

    print("In-order traversal:", bt.inorder_traversal())
    print("Pre-order traversal:", bt.preorder_traversal())
    print("Post-order traversal:", bt.postorder_traversal())

    print("Search 40:", bt.search(40))
    print("Search 100:", bt.search(100))

    bt.remove(50)
    print("In-order traversal after removing 50:", bt.inorder_traversal())