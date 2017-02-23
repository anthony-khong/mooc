#include <iostream>
#include <vector>
#include <map>
#include <string>

struct Edge {
    int key;
    int weight;
};

class Vertex {
    public:
        int key;
        std::vector<Edge> incoming;
        std::vector<Edge> outgoing;
        std::map<int,int> weights;
};

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
    std::string result = "Vector[";
    for (std::size_t i = 0; i < xs.size(); ++i) {
        result.append(to_str(xs[i]));
        if (i != (xs.size() - 1)) {
            result.append(", ");
        }
    }
    result.append("]\n");
    return result;
}

std::string vertex_to_str(Vertex vertex) {
    std::string key_str = to_str(vertex.key);
    std::string incoming_str = vector_to_str(vertex.incoming);
    std::string outgoing_str = vector_to_str(vertex.outgoing);

    return "";
}


int main() {
    Vertex vertex;
    vertex.key = 10;
    std::cout << vertex_to_str(vertex) << '\n';

    Edge edge;
    edge.key = 0;
    edge.weight = 1;
    std::cout << to_str(edge) << '\n';

    std::vector<Edge> x;
    x.push_back(edge);
    x.push_back(edge);
    std::cout << vector_to_str(x) << '\n';
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
