# python3

import sys

sys.setrecursionlimit(200000)

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
    def __init__(self, vertices, in_graph, reverse=False):
        self.explored = {k: False for k in vertices if in_graph[k]}
        self.post_order = []
        self.in_graph = in_graph
        self.reverse = reverse

    def pre_visit(self, key):
        self.explored[key] = True

    def post_visit(self, key):
        self.post_order.insert(0, key)

    def explore(self, vertices, key):
        self.pre_visit(key)
        if self.in_graph[key]:
            vertex = vertices[key]
            next_keys = self.get_keys_to_explore(vertex)
            for next_key in next_keys:
                if not self.explored[next_key]:
                    self.explore(vertices, next_key)
            self.post_visit(key)
        return self.post_order

    def get_keys_to_explore(self, vertex):
        next_keys = vertex.incoming if self.reverse else vertex.outgoing
        return [k for k in next_keys if self.in_graph[k]]

def find_post_ordered_keys_in_reverse_graph(vertices, in_graph):
    tracker = CycleTracker(vertices, in_graph, reverse=True)
    for key in vertices:
        if not tracker.explored[key]:
            tracker.explore(vertices, key)
    return tracker.post_order

def remove_vertices(in_graph, keys):
    for key in keys:
        in_graph[key] = False
    return in_graph

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    in_graph = {k: True for k in vertices}
    ordered_keys = find_post_ordered_keys_in_reverse_graph(vertices, in_graph)

    n_scc = 0
    tracker = CycleTracker(vertices, in_graph, reverse=False)
    for key in ordered_keys:
        if in_graph[key]:
            sink_keys = tracker.explore(vertices, key)
            in_graph = remove_vertices(in_graph, sink_keys)
            n_scc += 1
            tracker.post_order = []
    print(n_scc)


'''

4 4
1 2
4 1
2 3
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
