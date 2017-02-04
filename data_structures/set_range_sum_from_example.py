# python3

from sys import stdin

# Splay tree implementation

# Vertex of a splay tree
class Vertex:
    def __init__(self, key, sum, left, right, parent):
        (self.key, self.sum, self.left, self.right, self.parent) = (key, sum, left, right, parent)

    def print_keys(self):
        print(self.key)
        if self.right is not None:
            self.right.print_keys()
        if self.left is not None:
            self.left.print_keys()

def update(v):
  if v == None:
    return
  v.sum = v.key + (v.left.sum if v.left != None else 0) + (v.right.sum if v.right != None else 0)
  if v.left != None:
    v.left.parent = v
  if v.right != None:
    v.right.parent = v

def smallRotation(v):
  parent = v.parent
  if parent == None:
    return
  grandparent = v.parent.parent
  if parent.left == v:
    m = v.right
    v.right = parent
    parent.left = m
  else:
    m = v.left
    v.left = parent
    parent.right = m
  update(parent)
  update(v)
  v.parent = grandparent
  if grandparent != None:
    if grandparent.left == parent:
      grandparent.left = v
    else: 
      grandparent.right = v

def bigRotation(v):
  if v.parent.left == v and v.parent.parent.left == v.parent:
    # Zig-zig
    smallRotation(v.parent)
    smallRotation(v)
  elif v.parent.right == v and v.parent.parent.right == v.parent:
    # Zig-zig
    smallRotation(v.parent)
    smallRotation(v)    
  else: 
    # Zig-zag
    smallRotation(v)
    smallRotation(v)

# Makes splay of the given vertex and makes
# it the new root.
def splay(v):
  if v == None:
    return None
  while v.parent != None:
    if v.parent.parent == None:
      smallRotation(v)
      break
    bigRotation(v)
  return v

# Searches for the given key in the tree with the given root
# and calls splay for the deepest visited node after that.
# Returns pair of the result and the new root.
# If found, result is a pointer to the node with the given key.
# Otherwise, result is a pointer to the node with the smallest
# bigger key (next value in the order).
# If the key is bigger than all keys in the tree,
# then result is None.
def find(root, key): 
  v = root
  last = root
  next = None
  while v != None:
    if v.key >= key and (next == None or v.key < next.key):
      next = v    
    last = v
    if v.key == key:
      break    
    if v.key < key:
      v = v.right
    else: 
      v = v.left      
  root = splay(last)
  return (next, root)

def split(root, key):  
  (result, root) = find(root, key)  
  if result == None:    
    return (root, None)  
  right = splay(result)
  left = right.left
  right.left = None
  if left != None:
    left.parent = None
  update(left)
  update(right)
  return (left, right)

  
def merge(left, right):
  if left == None:
    return right
  if right == None:
    return left
  while right.left != None:
    right = right.left
  right = splay(right)
  right.left = left
  update(right)
  return right

  
# Code that uses splay tree to solve the problem
                                    
root = None

def insert(x):
    global root
    (left, right) = split(root, x)
    new_vertex = None
    if right == None or right.key != x:
        new_vertex = Vertex(x, x, None, None, None)  
    root = merge(merge(left, new_vertex), right)
  
def erase(x): 

    def next_(node):
        if node.right:
            return left_descendant(node.right)
        else:
            return right_ancestor(node)

    def left_descendant(node):
        while node.left:
            node = node.left
        return node

    def right_ancestor(node):
        while node.parent is not None:
            if node.key < node.parent.key:
                return node.parent
            else:
                node = node.parent
        return None

    global root
    node, _ = find_(root, x)
    if node is None:
        pass
    elif node.key != x:
        pass
    else:
        next_node = next_(node)
        if next_node:
            next_node = splay(next_node)
        node = splay(node)

        if node.right:
            assert node.right.left is None
            root = node.right
            root.parent = None
            root.left = node.left
            update(root)
        else:
            root = node.left
            if root:
                root.parent = None
            update(root)

def find_(node, key):
    v = node
    last = node
    next = None
    while v != None:
        if v.key >= key and (next == None or v.key < next.key):
            next = v    
        last = v
        if v.key == key:
            break    
        if v.key < key:
            v = v.right
        else: 
            v = v.left      
    return next, last

def search(x): 
    global root
    node, root = find(root, x)
    return node
  
def sum(fr, to): 
    global root
    (left, middle) = split(root, fr)
    (middle, right) = split(middle, to + 1)
    lsum = left.sum if left is not None else 0
    rsum = right.sum if right is not None else 0
    root = merge(left, merge(middle, right))
    root_sum = root.sum if root is not None else 0
    ans = root_sum - lsum - rsum
    return ans

logs = []
MODULO = 1000000001
n = int(stdin.readline())
lines = [stdin.readline().split() for _ in range(n)]
last_sum_result = 0
for j, line in enumerate(lines):
    if line[0] == '+':
        x = (int(line[1]) + last_sum_result) % MODULO
        logs.append('add ' + str(x))
        insert(x)
    elif line[0] == '-':
        x = (int(line[1]) + last_sum_result) % MODULO
        logs.append('del ' + str(x))
        erase(x)
    elif line[0] == '?':
        x = (int(line[1]) + last_sum_result) % MODULO
        logs.append('find ' + str(x))
        print('Found' if search(x) else 'Not found')
    elif line[0] == 's':
        l = (int(line[1]) + last_sum_result) % MODULO
        r = (int(line[2]) + last_sum_result) % MODULO
        logs.append('sum ' + str(l) + ' ' + str(r))
        res = sum(l, r)
        print(res)
        last_sum_result = res % MODULO

    print('-' * 20)
    print(str(j) + '   ' + str(logs[-1]))
    if root:
        root.print_keys()
    print('-' * 20)
