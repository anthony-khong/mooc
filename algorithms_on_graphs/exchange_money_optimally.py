# python3

from queue import Queue
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

def exchange_money_optimally(vertices, start):
    distances = {k: None for k in vertices}
    distances[start] = 0

    # Trim vertices so that we don't need to loop through edges that are not relevant
    reachables = find_all_reachables(vertices, start)
    vertices = {k: vertices[k] for k in reachables}

    # Find all shortest paths that are not in a negative cycle
    n_vertices = len(vertices)
    for _ in range(n_vertices - 1):
        relax_all_edges(distances, vertices)

    # Cancel out all vertices in a negative cycle
    relaxed = relax_all_edges(distances, vertices)
    print(relaxed)
    while relaxed:
        in_negative_cycle = find_all_reachables(vertices, relaxed[-1])
        print(in_negative_cycle)
        for key in in_negative_cycle:
            distances[key] = '-'
            relaxed.remove(key)

    # Star out all vertices that are not reachable
    for key, value in distances.items():
        if value is None:
            distances[key] = '*'

    return distances

def find_all_reachables(vertices, start):
    visited = {k: False for k in vertices}
    visited[start] = True

    reachables, queue = [start], Queue()
    queue.put(start)
    while queue.qsize():
        vertex = vertices[queue.get()]
        for next_key, _ in vertex.outgoing:
            if not visited[next_key]:
                reachables.append(next_key)
                queue.put(next_key)
                visited[next_key] = True
    return reachables

def relax_all_edges(distances, vertices):
    relaxed = []
    for origin, destination, weight in all_outgoing_edges(vertices):
        org_dist, dest_dist = distances[origin], distances[destination]
        if org_dist is None:
            pass
        elif dest_dist is None or dest_dist > org_dist + weight:
            distances[destination] = org_dist + weight
            relaxed.append(destination)
    return relaxed

def all_outgoing_edges(vertices):
    for origin in vertices.keys():
        for destination, weight in vertices[origin].outgoing:
            yield origin, destination, weight

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    [start] = parse_ints()
    result = exchange_money_optimally(vertices, start)
    for key in vertices:
        print(result[key])

'''

6 7
1 2 10
2 3 5
1 3 100
3 5 7
5 4 10
4 3 -18
6 1 -1
1

5 4
1 2 1
4 1 2
2 3 2
3 1 -5
4

'''
