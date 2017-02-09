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

def explore(vertices, key, controller):
    visited_bools = {k: False for k in vertices}
    recursive_explore(visited_bools, controller, vertices, key)
    return controller.get_result()

def recursive_explore(visited_bools, controller, vertices, key):
    visited_bools[key] = True
    controller.pre_visit(key)
    for neighbour in vertices[key].neighbours:
        if not visited_bools[neighbour]:
            recursive_explore(visited_bools, controller, vertices, neighbour)
    controller.post_visit(key)

class NeighbourTracker(object):
    def __init__(self, vertices):
        self.unvisited = list(vertices)

    def pre_visit(self, key):
        self.unvisited.remove(key)

    def post_visit(self, key):
        pass

    def get_result(self):
        return self.unvisited

def get_num_connected_components(vertices):
    tracker = NeighbourTracker(vertices)
    n_connected_components = 0
    while tracker.unvisited:
        n_connected_components += 1
        next_key = tracker.unvisited[0]
        explore(vertices, next_key, tracker)
    return n_connected_components

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    print(get_num_connected_components(vertices))

'''

4 2
1 2
3 2

'''
