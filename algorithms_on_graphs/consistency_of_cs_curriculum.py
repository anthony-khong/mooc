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
        self.post_count = {k: None for k in vertices}
        self.current_post_count = 0

    def pre_visit(self, key):
        self.explored[key] = True

    def post_visit(self, key):
        self.unvisited.remove(key)
        self.current_post_count = self.current_post_count + 1
        self.post_count[key] = self.current_post_count

    def explore(self, vertices, key):
        self.pre_visit(key)
        for next_key in vertices[key].outgoing:
            if not self.explored[next_key]:
                self.explore(vertices, next_key)
        self.post_visit(key)

    def get_result(self, vertices):
        for key, vertex in vertices.items():
            post_counts = [self.post_count[k] for k in vertex.outgoing]
            max_post_count = max(post_counts) if post_counts else 0
            if self.post_count[key] < max_post_count:
                return True
        return False

if __name__ == '__main__':
    vertices = parse_vertices_from_input()
    tracker = CycleTracker(vertices)
    while tracker.unvisited:
        next_key = tracker.unvisited[0]
        tracker.explore(vertices, next_key)
    print(int(tracker.get_result(vertices)))

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
