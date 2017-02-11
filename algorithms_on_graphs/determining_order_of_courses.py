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
        self.post_order = []

    def pre_visit(self, key):
        self.explored[key] = True

    def post_visit(self, key):
        self.post_order.insert(0, key)

    def explore(self, vertices, key):
        self.pre_visit(key)
        for next_key in vertices[key].outgoing:
            if not self.explored[next_key]:
                self.explore(vertices, next_key)
        self.post_visit(key)

    def get_result(self, vertices):
        return ' '.join(str(x) for x in self.post_order)

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    tracker = CycleTracker(vertices)
    for key in vertices:
        if not tracker.explored[key]:
            tracker.explore(vertices, key)
    print(tracker.get_result(vertices))

'''

4 3
1 2
4 1
3 1

4 1
3 1

5 7
2 1
3 2
3 1
4 3
4 1
5 2
5 3

'''
