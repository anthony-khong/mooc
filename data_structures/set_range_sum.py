# python3

from sys import stdin

# Splay tree implementation

# Vertex of a splay tree
class Vertex:
    def __init__(self, key, sum, left, right, parent):
        (self.key, self.sum, self.left, self.right, self.parent) = (key, sum, left, right, parent)

    def print_keys(self):
        print(self.keys)

    @property
    def keys(self):
        keys = [self.key]
        if self.left:
            keys += self.left.keys
        if self.right:
            keys += self.right.keys
        return keys

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
    global root
    result, root = find(root, x)
    if result and result.key == x:
        left, middle = split(root, x)
        middle, right = split(middle, x + 1)
        root = merge(left, right)
    else:
        pass

def search(x): 
    global root
    node, root = find(root, x)
    if node is None:
        return False
    else:
        return node.key == x
  
def sum(fr, to): 
    global root
    (left, middle) = split(root, fr)
    (middle, right) = split(middle, to + 1)
    ans = 0
    lsum = left.sum if left is not None else 0
    rsum = right.sum if right is not None else 0
    root = merge(left, merge(middle, right))
    root_sum = root.sum if root is not None else 0
    ans = root_sum - lsum - rsum
    return ans

logs, keys_hist = [], [[]]
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

    # if root:
        # keys_hist.append(root.keys)
# for j, (log, hist) in enumerate(zip(logs, keys_hist)):
    # print(('-' * 10) + str(j) + ('-' * 10))
    # print(log)
    # print(hist)
    # print('-' * 20)
