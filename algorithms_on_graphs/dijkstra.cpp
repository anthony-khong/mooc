#include <iostream>
#include <vector>
#include <map>
#include <string>

struct Edge {
    int key;
    int weight;
};

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

std::map<int,Vertex> parse_vertices() {
    int n_vertices, n_edges;
    std::cin >> n_vertices >> n_edges;
    std::map<int,Vertex> vertices;
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

int main() {
    std::map<int,Vertex> vertices = parse_vertices();
    for (auto& kv: vertices) {
        int key = kv.first;
        Vertex vertex = kv.second;
        std::cout << key << ": " << to_str(vertex) << std::endl;
    }
}

/*
#include <iostream>
#include <vector>
#include <queue>

using std::vector;
using std::queue;
using std::pair;
using std::priority_queue;

int distance(vector<vector<int> > &adj, vector<vector<int> > &cost, int s, int t) {
  //write your code her
  return -1;
}

int main() {
  int n, m;
  std::cin >> n >> m;
  vector<vector<int> > adj(n, vector<int>());
  vector<vector<int> > cost(n, vector<int>());
  for (int i = 0; i < m; i++) {
    int x, y, w;
    std::cin >> x >> y >> w;
    adj[x - 1].push_back(y - 1);
    cost[x - 1].push_back(w);
  }
  int s, t;
  std::cin >> s >> t;
  s--, t--;
  std::cout << distance(adj, cost, s, t);
}
*/
