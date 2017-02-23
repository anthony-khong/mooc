# python3

from queue import PriorityQueue
from collections import namedtuple

class Vertex(object):
    def __init__(self, key, incoming=None, outgoing=None):
        self.key = key
        self.incoming = incoming or []
        self.outgoing = outgoing or []
        self.weights = {}

    def __str__(self):
        return 'V({})(in={}, out={})'.format(
                self.key,
                self.incoming,
                self.outgoing
                )
    __repr__ = __str__

Edge = namedtuple('Edge', ['key', 'weight'])

def parse_ints():
    return [int(x) for x in input().split(' ')]

def parse_vertices_from_input():
    n_vertices, n_edges = parse_ints()
    vertices = {i + 1: Vertex(i + 1) for i in range(n_vertices)}
    edges = [parse_ints() for _ in range(n_edges)]
    for u, v, weight in edges:
        vertices[u].outgoing.append(Edge(v, weight))
        vertices[v].incoming.append(Edge(u, weight))
    return vertices

def min_cost(vertices, start, end):
    distances = {k: None for k in vertices}
    distances[start] = 0

    queue = PriorityQueue()
    queue.put((0, start))
    while queue.qsize():
        dist, key = queue.get()
        vertex = vertices[key]
        for next_key, next_weight in vertex.outgoing:
            old_dist = distances[next_key]
            new_dist = dist + next_weight
            if old_dist is None or old_dist > new_dist:
                distances[next_key] = new_dist
                queue.put((new_dist, next_key))
        if key == end:
            return distances[key]
    return -1

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    start, end = parse_ints()
    result = min_cost(vertices, start, end)
    print(result)

'''

4 4
1 2 1
4 1 2
2 3 2
1 3 5
1 3

5 9
1 2 4
1 3 2
2 3 2
3 2 1
2 4 2
3 5 4
5 4 1
2 5 3
3 4 4
1 5

3 3
1 2 7
1 3 5
2 3 2
3 2

'''

