# python3

class DirectedVertex(object):
    def __init__(self, key, incoming=None, outgoing=None):
        self.key = key
        self.incoming = incoming or []
        self.outgoing = outgoing or []

    def __str__(self):
        return 'V({})(in={}, out={})'.format(
                self.key,
                self.incoming,
                self.outgoing
                )
    __repr__ = __str__

def parse_ints():
    return [int(x) for x in input().split(' ')]

def parse_vertices_from_input():
    n_vertices, n_edges = parse_ints()
    vertices = {i + 1: DirectedVertex(i + 1) for i in range(n_vertices)}
    edges = [parse_ints() for _ in range(n_edges)]
    for u, v in edges:
        vertices[u].outgoing.append(v)
        vertices[v].incoming.append(u)
    return vertices

class CycleTracker(object):
    def __init__(self, vertices):
        self.explored = {k: False for k in vertices}
        self.unvisited = list(vertices)
        self.result = False

    def pre_visit(self, key):
        self.explored[key] = True

    def post_visit(self, key):
        self.unvisited.remove(key)

    def explore(self, vertices, key):
        print(key)
        self.pre_visit(key)
        for next_key in vertices[key].outgoing:
            if self.explored[next_key]:
                self.result = True
            if not self.explored[next_key]:
                self.explore(vertices, next_key)
        self.post_visit(key)

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    tracker = CycleTracker(vertices)
    while tracker.unvisited:
        next_key = tracker.unvisited[0]
        tracker.explore(vertices, next_key)
        if tracker.result:
            break
    print(int(tracker.result))

'''

4 4
1 2
4 1
2 3
3 1

5 7
1 2
2 3
1 3
3 4
1 4
2 5
3 5

'''
