# python3

import sys
import threading
sys.setrecursionlimit(10**7)
threading.stack_size(2**27)

def read_parent_ixs():
    _ = int(sys.stdin.readline())
    input_numbers = sys.stdin.readline().split()
    return list(map(int, input_numbers))

class Tree(object):
    def __init__(self, key):
        self.key = key
        self.parent = None
        self.children = []

    def set_parent(self, parent):
        self.parent = parent
        parent.children.append(self)

    def get_height(self):
        if self.children:
            return 1 + max([c.get_height() for c in self.children])
        else:
            return 1

    @classmethod
    def from_parent_ixs(cls, parent_ixs):
        nodes = [cls(i) for i in range(len(parent_ixs))]
        for child_ix, parent_ix in enumerate(parent_ixs):
            if parent_ix == -1:
                root = nodes[child_ix]
            else:
                child, parent = nodes[child_ix], nodes[parent_ix]
                child.set_parent(parent)
        return root

def main():
    parent_ixs = read_parent_ixs()
    tree = Tree.from_parent_ixs(parent_ixs)
    print(tree.slow_height())

threading.Thread(target=main).start()
