#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>
#include <map>
#include <queue>
#include <tuple>
#include <vector>

const double INF = std::numeric_limits<double>::max();
using WeightKey = std::tuple<double,int>;

WeightKey pair(double x, int y) {
    return std::make_tuple(x, y);
}

struct Point {
    int x;
    int y;
};
using PointMap = std::map<int,Point>;

// ********************************************************************** //

template <typename T>
std::string to_str(T x) {
    return std::to_string(x);
}

std::string to_str(Point point) {
    return "(" + to_str(point.x) + "," + to_str(point.y) + ")";
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

template <typename K, typename V>
void print_map(std::map<K,V> map) {
    for (auto& kv: map) {
        std::cout << to_str(kv.first) << ": " << to_str(kv.second) << '\n';
    }
}

template <typename T>
void print(T x) {
    std::cout << to_str(x) << '\n';
}

// ********************************************************************** //


Point create_point(int x, int y) {
    Point point;
    point.x = x;
    point.y = y;
    return point;
}

PointMap parse_points() {
    int n_points;
    std::cin >> n_points;
    PointMap points;
    for (int i = 0; i < n_points; ++i) {
        int x, y;
        std::cin >> x >> y;
        points[i] = create_point(x, y);
    }
    return points;
}

template <typename K, typename A, typename B>
std::map<K,A> initialise_map(std::map<K,B> base_map, A value) {
    std::map<K,A> new_map;
    for (auto& kv: base_map) {
        new_map[kv.first] = value;
    }
    return new_map;
}

double distance(Point a, Point b) {
    double dx = a.x - b.x;
    double dy = a.y - b.y;
    return std::sqrt(dx*dx + dy*dy);
}

std::priority_queue<WeightKey> initialise_queue(PointMap points, std::map<int,double> distances) {
    std::priority_queue<WeightKey> queue;
    for (auto& kv: points) {
        int key = kv.first;
        queue.push(pair(-distances[key], key));
    }
    return queue;
}

void print_queue(std::priority_queue<WeightKey> queue) {
    std::vector<WeightKey> pairs;
    int n = queue.size();
    for (int i = 0; i < n; ++i) {
        WeightKey p = queue.top();
        std::cout << "(" << std::get<0>(p) << "," << std::get<1>(p) << ")\n";
        pairs.push_back(p);
        queue.pop();
    }
    for (int i = 0; i < n; ++i) {
        queue.push(pairs[i]);
    }
}

std::map<int,int> min_dist_parent_map(PointMap points) {
    std::map<int,double> distances = initialise_map(points, INF);
    std::map<int,int> parents = initialise_map(points, -1);

    int start = 0;
    distances[start] = 0;
    std::priority_queue<WeightKey> queue = initialise_queue(points, distances);
    std::map<int,bool> in_queue = initialise_map(points, true);
    while (!queue.empty()) {
        int key = std::get<1>(queue.top());
        Point point = points[key];

        queue.pop();
        in_queue[key] = false;
        for (auto& kv: points) {
            int next_key = kv.first;
            Point next_point = kv.second;
            double next_dist = distance(point, next_point);
            if (in_queue[next_key] && (next_dist < distances[next_key])) {
                distances[next_key] = next_dist;
                parents[next_key] = key;
                queue.push(pair(-next_dist, next_key));
                in_queue[next_key] = true;
            }
        }
    }
    return parents;
}

double sum_distances(PointMap points, std::map<int,int> parents) {
    double sum_dist = 0.0;
    for (auto& kv: points) {
        int key = kv.first;
        Point point = kv.second;
        int parent = parents[key];
        if (parent != -1) {
            sum_dist += distance(point, points[parent]);
        }
    }
    return sum_dist;
}

int main() {
    PointMap points = parse_points();
    std::map<int,int> parent_map = min_dist_parent_map(points);
    std::cout << std::fixed;
    std::cout << std::setprecision(9);
    std::cout << sum_distances(points, parent_map);
}
