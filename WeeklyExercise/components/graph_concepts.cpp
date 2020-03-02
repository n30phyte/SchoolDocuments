#include <algorithm>
#include <fstream>
#include <iostream>
#include <queue>
#include <unordered_set>

#include "digraph.h"

void breadth_first_search(const Digraph &graph, const int startVertex, unordered_set<int> &visited) {
    queue<int> q;

    q.push(startVertex);

    while (!q.empty()) {
        int vertex = q.front();
        q.pop();

        // Go through each neighbor
        for (auto it = graph.neighbours(vertex); it != graph.endIterator(vertex); it++) {
            if (visited.find(*it) == visited.end()) {
                // Add to visited list
                visited.insert(*it);
                // add to queue of nodes to search next.
                q.push(*it);
            }
        }
    }
}

int count_components(Digraph *g) {
    vector<int> nodes = g->vertices();
    unordered_set<int> visited;

    unsigned int components = 0;

    for (auto node : nodes) {
        if (visited.find(node) == visited.end()) {
            // Not visited yet
            breadth_first_search(*g, node, visited);
            components++;
        }
    }

    return components;
}

Digraph *read_city_graph_undirected(char filename[]) {
    auto graph = new Digraph();

    std::ifstream file(filename);
    std::string line;
    // repeat while not EOF
    while (std::getline(file, line)) {
        if (line[0] == 'V') {
            // Vertex
            auto next_comma = line.find(',', 2);
            auto ID = std::stoi(line.substr(2, next_comma - 2));

            graph->addVertex(ID);
        } else {
            auto first_comma = line.find(',', 2);
            auto first_vertex = std::stoi(line.substr(2, first_comma - 2));
            auto second_comma = line.find(',', first_comma);
            auto second_vertex = std::stoi(line.substr(first_comma + 1, second_comma - 2));

            graph->addEdge(first_vertex, second_vertex);
            graph->addEdge(second_vertex, first_vertex);
        }
    }

    return graph;
}

int main(int argc, char *argv[]) {
    auto graph = read_city_graph_undirected(argv[1]);
    std::cout << count_components(graph) << std::endl;

    return 0;
}