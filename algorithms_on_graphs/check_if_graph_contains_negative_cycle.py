# python3

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

def check_for_negative_cycle(vertices):
    distances = {k: 0 for k in vertices}

    n_vertices = len(vertices)
    for _ in range(n_vertices - 1):
        relax_all_edges(distances, vertices)
    return int(relax_all_edges(distances, vertices))

def relax_all_edges(distances, vertices):
    is_updated = False
    for origin, destination, weight in all_outgoing_edges(vertices):
        org_dist, dest_dist = distances[origin], distances[destination]
        if dest_dist is None or dest_dist > org_dist + weight:
            distances[destination] = org_dist + weight
            is_updated = True
    return is_updated

def all_outgoing_edges(vertices):
    for origin in vertices.keys():
        for destination, weight in vertices[origin].outgoing:
            yield origin, destination, weight

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    result = check_for_negative_cycle(vertices)
    print(result)

'''

4 4
1 2 1
4 1 2
2 3 2
1 3 5

4 4
1 2 -5
4 1 2
2 3 2
3 1 1

'''
