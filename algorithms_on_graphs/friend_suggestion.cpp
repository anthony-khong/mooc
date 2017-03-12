#include <iostream>
#include <limits>
#include <map>
#include <queue>
#include <string>
#include <tuple>
#include <vector>

//const int INF = std::numeric_limits<int>::max();

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

class Vertex {
    public:
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
using Queries = std::vector<Query>;
Queries parse_queries() {
    int n_queries;
    std::cin >> n_queries;
    Queries queries;
    for (int i = 0; i < n_queries; ++i) {
        int u, v;
        std::cin >> u >> v;
        Query query = std::make_tuple(u, v);
        queries.push_back(query);
    }
    return queries;
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
    std::map<int,int> distances;
    std::vector<int> processed;
    std::priority_queue<WeightKeyPair> queue;

    void print() {
        std::cout << "\nDistances:\n" << to_str(distances);
        std::cout << "Processed:\n" << to_str(processed) << '\n';
        std::cout << "Queue:\n" << to_str(queue) << '\n';
    }

    bool is_queue_empty() {
        return queue.empty();
    }
};


int main() {
    Vertices vertices = parse_vertices();
    Queries queries = parse_queries();

    std::cout << to_str(vertices);
    std::cout << to_str(queries) << '\n';
}
