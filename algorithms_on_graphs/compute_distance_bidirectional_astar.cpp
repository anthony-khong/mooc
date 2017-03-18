#include <cmath>
#include <iostream>
#include <limits>
#include <queue>
#include <string>
#include <tuple>
#include <vector>

using Length = long long;
const Length INF = std::numeric_limits<Length>::max();

/***********************************************************************/
// Data Structutures
struct Edge {
    int key;
    Length weight;
};
using Edges = std::vector<Edge>;

struct Vertex {
    int key;
    int x;
    int y;
    Edges incoming;
    Edges outgoing;
};
using Vertices = std::vector<Vertex>;

using WeightKeyPair = std::tuple<Length, int>;

Edge create_edge(int key, Length weight);
Vertex create_singleton_vertex(int key, int x, int y);
Vertices parse_vertices();
Vertices reverse_graph(Vertices vertices);

class AStarTracker;

/***********************************************************************/
// Prints for Debugging
template <typename T> std::string to_str(T x);

std::string to_str(Edge edge);

std::string to_str(std::tuple<int,int> query);

template <typename T> std::string to_str(std::vector<T> xs);

std::string to_str(Vertex vertex);

template <typename T> std::string to_str(std::priority_queue<T> queue);

std::string to_str(AStarTracker tracker);

/***********************************************************************/
// Main Algorithm
Length euclidean_distance(Vertex &target_vertex, Vertex &current_vertex) {
    double dx = (double)target_vertex.x - (double)current_vertex.x;
    double dy = (double)target_vertex.y - (double)current_vertex.y;
    return std::sqrt(dx*dx + dy*dy);
}

class AStarTracker {
    public:
        Vertex start;
        Vertex end;
        Vertices vertices;
        std::vector<Length> distances;
        std::priority_queue<WeightKeyPair> queue;
        std::vector<int> visited;
        std::vector<int> processed;
        std::vector<bool> processed_flags;

    Vertex get_vertex(int key) { return vertices[key - 1]; }
    Length get_distance(int key) { return distances[key - 1]; }
    void set_distance(int key, Length dist) { distances[key - 1] = dist; }
    bool is_key_processed(int key) { return processed_flags[key - 1]; }

    int extract_top_of_queue() {
        WeightKeyPair top = queue.top();
        queue.pop();
        return std::get<1>(top);
    }

    void queue_pair(Length weight, int key) {
        queue.push(std::make_tuple(-weight, key));
    }

    void set_start_and_end_keys(int start_key, int end_key) {
        start = get_vertex(start_key);
        end = get_vertex(end_key);
        set_distance(start_key, 0);
        queue_pair(0, start_key);
        visited.push_back(start_key);
    }

    void clear_state() {
        for (int& key: visited) { set_distance(key, INF); }
        visited.clear();
        for (int& key: processed) { processed_flags[key - 1] = false; }
        processed.clear();
        std::priority_queue<WeightKeyPair> empty_queue;
        queue = empty_queue;
    }

    void process(int key) {
        Vertex vertex = get_vertex(key);
        Edges edges_to_explore = vertex.outgoing;
        for (auto& edge: edges_to_explore) { relax(vertex, edge); }
        processed.push_back(vertex.key);
        processed_flags[vertex.key - 1] = true;
    }

    void relax(Vertex &vertex, Edge &edge) {
        Length old_dist = get_distance(edge.key);
        Length base_dist = get_distance(vertex.key);
        Length weight = get_astar_weight(vertex, edge);
        Length new_dist = base_dist + weight;
        if (old_dist > new_dist) {
            set_distance(edge.key, new_dist);
            queue_pair(new_dist, edge.key);
            if (old_dist == INF) { visited.push_back(edge.key); }
        }
    }

    Length get_astar_weight(Vertex &origin_vertex, Edge &edge) {
        Vertex destination_vertex = get_vertex(edge.key);
        Length potential = get_potential(origin_vertex, destination_vertex);
        return edge.weight + potential;
    }

    Length get_real_distance(int key) {
        Vertex destination_vertex = get_vertex(key);
        return get_distance(key) - get_potential(start, destination_vertex);
    }

    Length get_potential(Vertex &origin_vertex, Vertex &destination_vertex) {
        Length pi_f_u = euclidean_distance(origin_vertex, end);
        Length pi_r_u = euclidean_distance(start, origin_vertex);
        Length pi_f_v = euclidean_distance(destination_vertex, end);
        Length pi_r_v = euclidean_distance(start, destination_vertex);
        Length p_u = (pi_f_u - pi_r_u) / 2;
        Length p_v = (pi_f_v - pi_r_v) / 2;
        return -p_u + p_v;
    }
};

AStarTracker create_tracker(Vertices &vertices) {
    AStarTracker tracker;
    tracker.vertices = vertices;
    int n_vertices = vertices.size();
    for (int i = 0; i < n_vertices; ++i) {
        tracker.distances.push_back(INF);
        tracker.processed_flags.push_back(false);
    }
    return tracker;
}

int minimum_distance(AStarTracker &ftracker, AStarTracker &btracker) {
    Length min_dist = INF;
    for (int& key: ftracker.processed) {
        Length fdist = ftracker.get_distance(key);
        Length bdist = btracker.get_distance(key);
        if ((fdist != INF) && (bdist != INF)) {
            Length sum_dist = fdist + bdist;
            if (sum_dist < min_dist) { min_dist = sum_dist; }
        }
    }
    Length final_potential = ftracker.get_potential(ftracker.start, ftracker.end);
    return min_dist - final_potential;
}

int bidirectional_astar(AStarTracker &ftracker, AStarTracker &btracker) {
    while ((!ftracker.queue.empty()) && (!btracker.queue.empty())) {
        int fkey = ftracker.extract_top_of_queue();
        ftracker.process(fkey);
        if (btracker.is_key_processed(fkey)) {
            return minimum_distance(ftracker, btracker);
        }

        int bkey = btracker.extract_top_of_queue();
        btracker.process(bkey);
        if (ftracker.is_key_processed(bkey)) {
            return minimum_distance(ftracker, btracker);
        }
    }
    return -1;
}

int main() {
    Vertices vertices = parse_vertices();
    Vertices rvertices = reverse_graph(vertices);
    AStarTracker ftracker = create_tracker(vertices);
    AStarTracker btracker = create_tracker(rvertices);

    int n_queries;
    std::cin >> n_queries;
    for (int i = 0; i < n_queries; ++i) {
        int start, end;
        std::cin >> start >> end;

        ftracker.set_start_and_end_keys(start, end);
        btracker.set_start_and_end_keys(end, start);
        Length min_dist = bidirectional_astar(ftracker, btracker);
        ftracker.clear_state();
        btracker.clear_state();

        //for (int j = 0; j < 10; ++j) {
            //std::cout << "j -> " << vertices[j].outgoing[0].key << '\n';
            //std::cout << "j -> " << rvertices[j].outgoing[0].key << '\n';
            //Vertex vertex = vertices[j];
            //Edge edge = vertices[j].outgoing[0];
            //Vertex rvertex = vertices[edge.key - 1];
            //Edge redge;
            //redge.key = vertex.key;
            //redge.weight = edge.weight;

            //Length fpot = ftracker.get_astar_weight(vertex, edge);
            //Length rpot = btracker.get_astar_weight(rvertex, redge);
            //std::cout << "Sum pot: " << fpot + rpot - 2*edge.weight<< '\n';
        //}

        std::cout << min_dist;
        if (i != (n_queries - 1)) { std::cout << '\n'; }
    }

}

/*
6854
2363
2139
4679
6078
9935
8037
5830
3419
10983
*/

/***********************************************************************/
Edge create_edge(int key, Length weight) {
    Edge edge;
    edge.key = key;
    edge.weight = weight;
    return edge;
}

Vertex create_singleton_vertex(int key, int x, int y) {
    Vertex vertex;
    vertex.key = key;
    vertex.x = x;
    vertex.y = y;
    Edges incoming, outgoing;
    return vertex;
}

Vertices parse_vertices() {
    int n_vertices, n_edges;
    std::cin >> n_vertices >> n_edges;
    Vertices vertices;
    for (int i = 0; i < n_vertices; ++i) {
        int x, y;
        std::cin >> x >> y;
        Vertex v = create_singleton_vertex(i + 1, x, y);
        vertices.push_back(v);
    }
    for (int j = 0; j < n_edges; ++j) {
        int u, v;
        Length weight;
        std::cin >> u >> v >> weight;
        vertices[u - 1].outgoing.push_back(create_edge(v, weight));
        vertices[v - 1].incoming.push_back(create_edge(u, weight));
    }
    return vertices;
}

Vertices reverse_graph(Vertices vertices) {
    for (auto& vertex: vertices) {
        Edges temp = vertex.outgoing;
        vertex.outgoing = vertex.incoming;
        vertex.incoming = temp;
    }
    return vertices;
}

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

template <typename A, typename B>
std::string to_str(std::tuple<A,B> query) {
    A u = std::get<0>(query);
    B v = std::get<1>(query);
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
    std::string coord_str = to_str(vertex.x) + ", " + to_str(vertex.y);
    std::string in_str = to_str(vertex.incoming);
    std::string out_str = to_str(vertex.outgoing);
    return "V(" + key_str + ")(" + coord_str + ")(in=" + in_str + ", out=" + out_str + ")";
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

std::string to_str(AStarTracker tracker) {
    std::string out = "\n****************************************\n";
    out += "\nDistances:\n" + to_str(tracker.distances);
    out += "Queue:\n" + to_str(tracker.queue) + '\n';
    out += "Visited:\n" + to_str(tracker.visited);
    out += "\n****************************************\n";
    return out;
}

/***********************************************************************/
