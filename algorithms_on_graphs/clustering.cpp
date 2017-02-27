#include <cmath>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <vector>

// ********************************************************************** //

template <typename T>
std::string to_str(std::vector<T> xs) {
    std::string result = "[";
    for (std::size_t i = 0; i < xs.size(); ++i) {
        result.append(std::to_string(xs[i]));
        if (i != (xs.size() - 1)) {
            result.append(", ");
        }
    }
    result.append("]");
    return result;
}

template <typename T>
void print(std::vector<T> xs) {
    std::cout << to_str(xs) << '\n';
}

class DisjointSet {
    public:
        int size;
        std::vector<int> parents;
        std::vector<int> ranks;
};

DisjointSet create_disjoint_set(int size) {
    std::vector<int> parents;
    std::vector<int> ranks;
    for (int i = 0; i < size; ++i) {
        parents.push_back(i);
        ranks.push_back(0);
    }

    DisjointSet dset;
    dset.size = size;
    dset.parents = parents;
    dset.ranks = ranks;
    return dset;
}

int find(DisjointSet dset, int i){
    if (dset.parents[i] != i) {
        dset.parents[i] = find(dset, dset.parents[i]);
    }
    return dset.parents[i];
}

void merge(DisjointSet& dset, int i, int j) {
    int parent_i = find(dset, i);
    int parent_j = find(dset, j);
    int rank_i = dset.ranks[i];
    int rank_j = dset.ranks[j];
    if (parent_i != parent_j) {
        if (rank_i > rank_j) {
            dset.parents[parent_j] = parent_i;
        } else {
            dset.parents[parent_i] = parent_j;
            if (rank_i == rank_j) {
                dset.ranks[parent_j] = rank_j + 1;
            }
        }
    }
}

struct Point {
    int x;
    int y;
};
using Points = std::vector<Point>;

Point create_point(int x, int y) {
    Point point;
    point.x = x;
    point.y = y;
    return point;
}

Points parse_points() {
    int n_points;
    std::cin >> n_points;
    Points points;
    for (int i = 0; i < n_points; ++i) {
        int x, y;
        std::cin >> x >> y;
        points.push_back(create_point(x, y));
    }
    return points;
}

struct Edge {
    double distance;
    int from;
    int to;
};
using Edges = std::vector<Edge>;

Edge create_edge(double distance, int from, int to) {
    Edge edge;
    edge.distance = distance;
    edge.from = from;
    edge.to = to;
    return edge;
}

double get_distance(Point a, Point b) {
    double dx = a.x - b.x;
    double dy = a.y - b.y;
    return std::sqrt(dx*dx + dy*dy);
}

Edges create_edges(Points points) {
    Edges edges;
    int n_points = points.size();
    for (int i = 0; i < n_points; ++i) {
        for (int j = 0; j < i; ++j) {
            double distance = get_distance(points[i], points[j]);
            edges.push_back(create_edge(distance, i, j));
        }
    }
    return edges;
}

bool compare_distance(Edge u, Edge v) {
    return u.distance < v.distance;
}

Edges get_sorted_edges(Points points) {
    Edges edges = create_edges(points);
    std::sort(edges.begin(), edges.end(), compare_distance);
    return edges;
}

// ********************************************************************** //

double min_distance_given_cluster(Points points, int min_clusters) {
    Edges edges = get_sorted_edges(points);
    DisjointSet dset = create_disjoint_set(points.size());
    int n_clusters = points.size();
    for (Edge edge: edges) {
        if (find(dset, edge.from) != find(dset, edge.to)) {
            merge(dset, edge.from, edge.to);
            n_clusters -= 1;
        }
        if (n_clusters < min_clusters) {
            return edge.distance;
        }
    }
    return 0.0;
}

int main() {
    Points points = parse_points();
    int min_clusters;
    std::cin >> min_clusters;
    double min_distance = min_distance_given_cluster(points, min_clusters);
    std::cout << std::fixed << std::setprecision(9) << min_distance;
}
