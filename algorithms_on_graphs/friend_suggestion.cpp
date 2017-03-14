#include <iostream>
#include <limits>
#include <map>
#include <queue>
#include <string>
#include <tuple>
#include <vector>

const int INF = std::numeric_limits<int>::max();

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
using Vertices = std::map<int,Vertex>;

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
    for (int i = 0; i < n_vertices; ++i) {
        vertices[i + 1] = create_singleton_vertex(i + 1);
    }
    for (int j = 0; j < n_edges; ++j) {
        int u, v, weight;
        std::cin >> u >> v >> weight;
        vertices[u].outgoing.push_back(create_edge(v, weight));
        vertices[v].incoming.push_back(create_edge(u, weight));
    }
    return vertices;
}

using Query = std::tuple<int,int>;
Query parse_query() {
    int u, v;
    std::cin >> u >> v;
    Query query = std::make_tuple(u, v);
    return query;
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

template <typename K, typename V>
std::string to_str(std::map<K,V> map) {
    std::string result = "";
    for (auto& kv: map) {
        result = result + to_str(kv.first) + ": " + to_str(kv.second) + "\n";
    }
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
    std::map<int,int> distances;
    std::vector<int> visited;
    std::vector<int> processed;
    std::priority_queue<WeightKeyPair> queue;
    bool reverse;

    void print() {
        std::cout << "\nDistances:\n" << to_str(distances);
        std::cout << "Processed:\n" << to_str(processed) << '\n';
        std::cout << "Queue:\n" << to_str(queue) << '\n';
    }

    void process(Vertex vertex) {
        Edges edges_to_explore;
        if (reverse) { edges_to_explore = vertex.incoming; }
        else { edges_to_explore = vertex.outgoing; }
        int distance = distances[vertex.key];

        for (auto& next_edge: edges_to_explore) {
            int next_key = next_edge.key;
            int next_weight = next_edge.weight;
            int old_dist = distances[next_key];
            int new_dist = distance + next_weight;
            if (old_dist > new_dist) {
                distances[next_key] = new_dist;
                push_pair(new_dist, next_key);
                visited.push_back(next_key);
            }
        }
        processed.push_back(vertex.key);
    }

    bool is_key_processed(int key) {
        for (auto& k: processed) { if (k == key) { return true; } }
        return false;
    }

    bool is_queue_empty() { return queue.empty(); }

    int extract_min() {
        WeightKeyPair top = queue.top();
        queue.pop();
        return std::get<1>(top);
    }

    void push_pair(int weight, int key) {
        queue.push(std::make_tuple(-weight, key));
    }

    void set_init_vertex(int key) {
        start = key;
        distances[key] = 0;
        push_pair(0, key);
        visited.push_back(key);
    }

    void clear() {
        for (int& key: visited) { distances[key] = INF; }
        std::vector<int> empty_processed;
        processed = empty_processed;
        std::priority_queue<WeightKeyPair> empty_queue;
        queue = empty_queue;
        std::vector<int> empty_visited;
        visited = empty_visited;
    }

};

template <typename K, typename A, typename B>
std::map<K,A> initialise_map(std::map<K,B> base_map, A value) {
    std::map<K,A> new_map;
    for (auto& kv: base_map) { new_map[kv.first] = value; }
    return new_map;
}

DijkstraTracker create_tracker(bool reverse, Vertices vertices) {
    DijkstraTracker tracker;
    tracker.reverse = reverse;
    tracker.distances = initialise_map(vertices, INF);
    return tracker;
}

int minimum_distance(DijkstraTracker ftracker, DijkstraTracker btracker) {
    std::vector<int> processed;
    if (ftracker.processed.size() < btracker.processed.size()) {
        processed = ftracker.processed;
    } else {
        processed = btracker.processed;
    }

    int min_dist = INF;
    for (int& key: processed) {
        int fdist = ftracker.distances[key];
        int bdist = btracker.distances[key];
        if ((fdist != INF) && (bdist != INF)) {
            int sum_dist = fdist + bdist;
            if (sum_dist < min_dist) {
                min_dist = sum_dist;
            }
        }
    }
    return min_dist;
}

int bidirectional_dijkstra(Vertices vertices, DijkstraTracker &ftracker, DijkstraTracker &btracker) {
    while ((!ftracker.is_queue_empty()) && (!btracker.is_queue_empty())) {
        int fkey = ftracker.extract_min();
        ftracker.process(vertices[fkey]);
        if (btracker.is_key_processed(fkey)) {
            return minimum_distance(ftracker, btracker);
        }

        int bkey = btracker.extract_min();
        btracker.process(vertices[bkey]);
        if (ftracker.is_key_processed(bkey)) {
            return minimum_distance(ftracker, btracker);
        }
    }
    return -1;
}

// TODO: avoid any kind of O(N) operation!
int main() {
    Vertices vertices = parse_vertices();
    DijkstraTracker ftracker = create_tracker(false, vertices);
    DijkstraTracker btracker = create_tracker(true, vertices);

    int n_queries;
    std::cin >> n_queries;
    for (int i = 0; i < n_queries; ++i) {
        Query query = parse_query();
        int start = std::get<0>(query);
        int end = std::get<1>(query);

        ftracker.set_init_vertex(start);
        btracker.set_init_vertex(end);
        int min_dist = bidirectional_dijkstra(vertices, ftracker, btracker);

        //std::cout << "********************\n";
        //std::cout << "Forward Tracker:\n";
        //ftracker.print();
        //std::cout << "********************\n";
        //std::cout << "********************\n";
        //std::cout << "Back Tracker:\n";
        //btracker.print();
        //std::cout << "********************\n";

        ftracker.clear();
        btracker.clear();

        //std::cout << "********************\n";
        //std::cout << "Forward Tracker:\n";
        //ftracker.print();
        //std::cout << "********************\n";
        //std::cout << "********************\n";
        //std::cout << "Back Tracker:\n";
        //btracker.print();
        //std::cout << "********************\n";

        std::cout << min_dist;
        if (i != (n_queries - 1)) {
            std::cout << '\n';
        }
    }
}
