# python3

from queue import Queue

class Vertex(object):
    def __init__(self, key, neighbours=None):
        self.key = key
        self.neighbours = neighbours or []

    def __str__(self):
        return 'V({}){}'.format(self.key, self.neighbours)
    __repr__ = __str__

def parse_ints():
    return [int(x) for x in input().split(' ')]

def parse_vertices_from_input():
    n_vertices, n_edges = parse_ints()
    vertices = {i + 1: Vertex(i + 1) for i in range(n_vertices)}
    edges = [parse_ints() for _ in range(n_edges)]
    for u, v in edges:
        vertices[u].neighbours.append(v)
        vertices[v].neighbours.append(u)
    return vertices

def is_graph_bipartite(vertices):
    colours = {k: None for k in vertices}
    start = 1
    colours[start] = 'white'

    queue = Queue()
    queue.put(start)
    while queue.qsize():
        key = queue.get()
        vertex = vertices[key]
        colour = colours[key]
        for neighbour in vertex.neighbours:
            if colours[neighbour] is None:
                colours[neighbour] = flip_colour(colour)
                queue.put(neighbour)
            elif colours[neighbour] == colour:
                return 0
    return 1

def flip_colour(clr):
    return 'black' if clr == 'white' else 'white'

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    result = is_graph_bipartite(vertices)
    print(result)

'''

4 4
1 2
4 1
2 3
3 1

5 4
5 2
4 2
3 4
1 4

'''
