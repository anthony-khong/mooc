# python3

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

def explore(vertices, key):
    visited_bools = {k: False for k in vertices}
    visited_vertices = []
    recursive_explore(visited_bools, visited_vertices, vertices, key)
    return visited_vertices

def recursive_explore(visited_bools, visited_vertices, vertices, key):
    visited_bools[key] = True
    visited_vertices.append(key)
    for neighbour in vertices[key].neighbours:
        if not visited_bools[neighbour]:
            recursive_explore(visited_bools, visited_vertices, vertices,
                              neighbour)

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    u, v = parse_ints()
    is_connected = v in explore(vertices, u)
    print(int(is_connected))

'''

4 4
1 2
3 2
4 3
1 4

4 2
1 2
3 2
1 4

'''
