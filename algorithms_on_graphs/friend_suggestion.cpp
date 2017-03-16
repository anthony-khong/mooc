#include <iostream>
#include <limits>
#include <queue>
#include <string>
#include <tuple>
#include <vector>

const long long INF = std::numeric_limits<long long>::max() / 4;

struct Edge {
    int key;
    int weight;
};
using Edges = std::vector<Edge>;

Edge create_edge(int key, int weight) {
    Edge edge;
    edge.key = key;
    edge.weight = weight;
    return edge;
}

struct Vertex {
    int key;
    Edges incoming;
    Edges outgoing;
};
using Vertices = std::vector<Vertex>;

Vertex create_singleton_vertex(int key) {
    Vertex vertex;
    vertex.key = key;
    Edges incoming, outgoing;
    return vertex;
}

Vertices parse_vertices() {
    int n_vertices, n_edges;
    std::cin >> n_vertices >> n_edges;
    Vertices vertices;
    vertices.reserve(n_vertices);
    for (int i = 0; i < n_vertices; ++i) {
        Vertex v = create_singleton_vertex(i + 1);
        vertices.push_back(v);
    }
    for (int j = 0; j < n_edges; ++j) {
        int u, v, weight;
        std::cin >> u >> v >> weight;
        vertices[u - 1].outgoing.push_back(create_edge(v, weight));
        vertices[v - 1].incoming.push_back(create_edge(u, weight));
    }
    return vertices;
}

using WeightKeyPair = std::tuple<int, int>;

/***********************************************************************/

template <typename T>
std::string to_str(T x) {
    return std::to_string(x);
}

std::string to_str(Edge edge) {
    std::string key_str = to_str(edge.key);
    std::string weight_str = to_str(edge.weight);
    return "(key=" + key_str + ", weight=" + weight_str + ")";
}

std::string to_str(std::tuple<int,int> query) {
    int u = std::get<0>(query);
    int v = std::get<1>(query);
    std::string result = "(" + to_str(u) + "," + to_str(v) + ")";
    return result;
}

template <typename T>
std::string to_str(std::vector<T> xs) {
    std::string result = "[";
    for (std::size_t i = 0; i < xs.size(); ++i) {
        result.append(to_str(xs[i]));
        if (i != (xs.size() - 1)) {
            result.append(", ");
        }
    }
    result.append("]");
    return result;
}

std::string to_str(Vertex vertex) {
    std::string key_str = to_str(vertex.key);
    std::string in_str = to_str(vertex.incoming);
    std::string out_str = to_str(vertex.outgoing);
    return "V(" + key_str + ")(in=" + in_str + ", out=" + out_str + ")";
}

template <typename T>
std::string to_str(std::priority_queue<T> queue) {
    std::string result = "Q[";
    std::vector<T> temporary_vector;
    int q_size = queue.size();
    for (int i = 0; i < q_size; ++i) {
        T x = queue.top();
        result = result + to_str(x);
        if (i != (q_size - 1)) {
            result = result + ", ";
        }
        temporary_vector.push_back(x);
        queue.pop();
    }
    result = result + "]";
    for (int i = 0; i < q_size; ++i) {
        queue.push(temporary_vector[i]);
    }
    return result;
}

/***********************************************************************/
struct DijkstraTracker {
    int start;
    std::vector<long long> distances;
    std::vector<int> visited;
    std::vector<int> processed;
    std::vector<bool> processed_flags;
    std::priority_queue<WeightKeyPair> queue;
    bool reverse;

    void print() {
        std::cout << "Start: " << start << '\n';
        std::cout << "\nDistances:\n" << to_str(distances);
        std::cout << "Visited:\n" << to_str(visited);
        std::cout << "Processed:\n" << to_str(processed) << '\n';
        std::cout << "Queue:\n" << to_str(queue) << '\n';
        std::cout << "Reverse: " << reverse << '\n';
    }

    long long get_distance(int key) { return distances[key - 1]; }

    void process(Vertex& vertex) {
        Edges edges_to_explore;
        if (reverse) { edges_to_explore = vertex.incoming; }
        else { edges_to_explore = vertex.outgoing; }
        long long distance = get_distance(vertex.key);

        for (auto& next_edge: edges_to_explore) {
            int next_key = next_edge.key;
            int next_weight = next_edge.weight;
            long long old_dist = get_distance(next_key);
            int new_dist = distance + next_weight;
            if (old_dist > new_dist) {
                distances[next_key - 1] = new_dist;
                queue_pair(new_dist, next_key);
                visited.push_back(next_key);
            }
        }
        processed.push_back(vertex.key);
        processed_flags[vertex.key - 1] = true;
    }

    bool is_key_processed(int key) { return processed_flags[key - 1]; }

    int extract_min() {
        WeightKeyPair top = queue.top();
        queue.pop();
        return std::get<1>(top);
    }

    void queue_pair(int weight, int key) {
        queue.push(std::make_tuple(-weight, key));
    }

    void set_init_vertex(int key) {
        start = key;
        distances[key - 1] = 0;
        queue_pair(0, key);
        visited.push_back(key);
    }

    void clear() {
        for (int& key: visited) { distances[key - 1] = INF; }
        for (int& key: processed) { processed_flags[key - 1] = false; }
        processed.clear();
        visited.clear();
        std::priority_queue<WeightKeyPair> empty_queue;
        queue = empty_queue;
    }
};

DijkstraTracker create_tracker(int n_vertices, bool reverse) {
    DijkstraTracker tracker;
    tracker.reverse = reverse;

    tracker.distances.reserve(n_vertices);
    tracker.visited.reserve(n_vertices);
    tracker.processed.reserve(n_vertices);
    tracker.processed_flags.reserve(n_vertices);
    for (int key = 1; key <= n_vertices; ++key) {
        tracker.distances.push_back(INF);
        tracker.processed_flags.push_back(false);
    }
    return tracker;
}

int minimum_distance(DijkstraTracker &ftracker, DijkstraTracker &btracker) {
    std::vector<int> processed;
    if (ftracker.processed.size() < btracker.processed.size()) {
        processed = ftracker.processed;
    } else {
        processed = btracker.processed;
    }

    long long min_dist = INF;
    for (int& key: processed) {
        long long fdist = ftracker.get_distance(key);
        long long bdist = btracker.get_distance(key);
        if ((fdist < INF) && (bdist < INF)) {
            long long sum_dist = fdist + bdist;
            if (sum_dist < min_dist) { min_dist = sum_dist; }
        }
    }
    return min_dist;
}

int bidirectional_dijkstra(Vertices &vertices, DijkstraTracker &ftracker, DijkstraTracker &btracker) {
    while ((!ftracker.queue.empty()) && (!btracker.queue.empty())) {
        int fkey = ftracker.extract_min();
        ftracker.process(vertices[fkey - 1]);
        if (btracker.is_key_processed(fkey)) {
            return minimum_distance(ftracker, btracker);
        }

        int bkey = btracker.extract_min();
        btracker.process(vertices[bkey - 1]);
        if (ftracker.is_key_processed(bkey)) {
            return minimum_distance(ftracker, btracker);
        }
    }
    return -1;
}

int main() {
    Vertices vertices = parse_vertices();
    DijkstraTracker ftracker = create_tracker(vertices.size(), false);
    DijkstraTracker btracker = create_tracker(vertices.size(), true);

    int n_queries;
    std::cin >> n_queries;
    for (int i = 0; i < n_queries; ++i) {
        int start, end;
        std::cin >> start >> end;

        int min_dist;
        if (start == end) {
            min_dist = 0;
        } else {
            ftracker.set_init_vertex(start);
            btracker.set_init_vertex(end);
            min_dist = bidirectional_dijkstra(vertices, ftracker, btracker);
            ftracker.clear();
            btracker.clear();
        }

        std::cout << min_dist;
        if (i != (n_queries - 1)) {
            std::cout << '\n';
        }
    }
}

/*

0
667
677
700
622
118
0
325
239
11

*/
