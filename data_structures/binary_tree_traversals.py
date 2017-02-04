# python3

import sys, threading
sys.setrecursionlimit(10**6) # max depth of recursion
threading.stack_size(2**27)  # new thread will get stack of such size

class BinaryTree(object):
    def __init__(self, key, left, right):
        self.key = key
        self.left = left
        self.right = right

    @classmethod
    def from_ixs(cls, ixs):
        subtrees = [cls.stub(i) for i, _, _ in ixs]
        for subtree, (_, left_ix, right_ix) in zip(subtrees, ixs):
            if left_ix != -1:
                subtree.left = subtrees[left_ix]
            if right_ix != -1:
                subtree.right = subtrees[right_ix]
        return subtrees[0]

    @classmethod
    def stub(cls, key):
        empty_right = cls(None, None, None)
        empty_left = cls(None, None, None)
        return cls(key, empty_right, empty_left)

def collect_in_order(tree, output):
    if tree.key is None:
        pass
    else:
        collect_in_order(tree.left, output)
        output.append(str(tree.key))
        collect_in_order(tree.right, output)
    return output

def collect_pre_order(tree, output):
    if tree.key is None:
        return []
    else:
        output.append(str(tree.key))
        collect_pre_order(tree.left, output)
        collect_pre_order(tree.right, output)
    return output

def collect_post_order(tree, output):
    if tree.key is None:
        return []
    else:
        collect_post_order(tree.left, output)
        collect_post_order(tree.right, output)
        output.append(str(tree.key))
    return output

def main():
    n_nodes = int(input())
    parse_line = lambda: [int(x) for x in input().split(' ')]
    ixs = [parse_line() for _ in range(n_nodes)]
    tree = BinaryTree.from_ixs(ixs)

    print(' '.join(collect_in_order(tree, [])))
    print(' '.join(collect_pre_order(tree, [])))
    print(' '.join(collect_post_order(tree, [])))

threading.Thread(target=main).start()

'''
"5"
"4 1 2"
"2 3 4"
"5 -1 -1"
"1 -1 -1"
"3 -1 -1"

1 2 3 4 5
4 2 1 3 5
1 3 2 5 4

10
0 7 2
10 -1 -1
20 -1 6
30 8 9
40 3 -1
50 -1 -1
60 1 -1
70 5 4
80 -1 -1
90 -1 -1
'''
