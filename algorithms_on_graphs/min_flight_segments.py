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

def min_distance(vertices, start, end):
    distances = {k: None for k in vertices}
    distances[start] = 0

    queue = Queue()
    queue.put(start)
    while queue.qsize():
        vertex = vertices[queue.get()]
        for neighbour in vertex.neighbours:
            if distances[neighbour] is None:
                distances[neighbour] = distances[vertex.key] + 1
                queue.put(neighbour)
            if neighbour == end:
                return distances[neighbour]
    return -1

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    start, end = parse_ints()
    result = min_distance(vertices, start, end)
    print(result)

'''

5 4
5 2
1 3
3 4
1 4
3 5

4 4
1 2
4 1
2 3
3 1
2 4


'''
