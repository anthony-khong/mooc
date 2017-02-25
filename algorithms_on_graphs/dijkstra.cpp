#include <iostream>
#include <vector>
#include <map>
#include <string>

struct Edge {
    int key;
    int weight;
};
using EdgeVector = std::vector<Edge>;

Edge create_edge(int key, int weight) {
    Edge edge;
    edge.key = key;
    edge.weight = weight;
    return edge;
}

class Vertex {
    public:
        int key;
        std::vector<Edge> incoming;
        std::vector<Edge> outgoing;
};
using VertexMap = std::map<int,Vertex>;

Vertex create_singleton_vertex(int key) {
    Vertex vertex;
    vertex.key = key;
    std::vector<Edge> incoming, outgoing;
    return vertex;
}

template <typename T>
std::string to_str(T x) {
    return std::to_string(x);
}

std::string to_str(Edge edge) {
    std::string key_str = to_str(edge.key);
    std::string weight_str = to_str(edge.weight);
    return "(key=" + key_str + ", weight=" + weight_str + ")";
}

template <typename T>
std::string vector_to_str(std::vector<T> xs) {
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
    std::string incoming_str = vector_to_str(vertex.incoming);
    std::string outgoing_str = vector_to_str(vertex.outgoing);
    return "V(" + key_str + ")(in=" + incoming_str + ", out=" + outgoing_str + ")";
}

template <typename K, typename V>
void print_map(std::map<K,V> map) {
    for (auto& kv: map) {
        std::cout << to_str(kv.first) << ": " << to_str(kv.second) << '\n';
    }
}

VertexMap parse_vertices() {
    int n_vertices, n_edges;
    std::cin >> n_vertices >> n_edges;
    VertexMap vertices;
    for (int i = 0; i < n_vertices; ++i) {
        vertices[i] = create_singleton_vertex(i + 1);
    }
    for (int j = 0; j < n_edges; ++j) {
        int u, v, weight;
        std::cin >> u >> v >> weight;
        vertices[u].outgoing.push_back(create_edge(v, weight));
        vertices[v].incoming.push_back(create_edge(u, weight));
    }
    return vertices;
}

//int min_cost(VertexMap vertices, int start, int end) {
    //return 10;
//}

int main() {
    VertexMap vertices = parse_vertices();
    print_map(vertices);

    //int start, end;
    //std::cin >> start >> end;
    //int result = min_cost(vertices, start, end);
    //std::cout << result;

}
