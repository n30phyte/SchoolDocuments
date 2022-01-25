// A template for the Graph Concepts Exercise in C++.
#include <algorithm>
#include <fstream>
#include <iostream>
#include <queue>
#include <string>
#include <unordered_set>

#include "digraph.h"

void bfs(Digraph &g, int node, unordered_set<int> &visited) {
  // queue of nodes to check out
  queue<int> q;

  // Add initial node
  q.push(node);

  // Start loop
  while (!q.empty()) {
    int vertex = q.front();
    q.pop();

    // Go through each neighbor
    for (auto it = g.neighbours(vertex); it != g.endIterator(vertex); it++) {
      if (visited.find(*it) == visited.end()) {
        // Add to visited list
        // Using unordered_set to keep track of visited and adding to that is easier than modifying node list
        // and risking going out of bounds
        visited.insert(*it);
        // add to queue of nodes to search next.
        q.push(*it);
      }
    }
  }
}

/**
 * Counts the number of connected components for a selected graph.
 *
 * Uses BFS implemented as part of the function instead of being in a seperate function.
 *
 * @param g Pointer to the graph to be searched
 * @return Count of connected components
 */
int count_components(Digraph *g) {
  vector<int> nodes = g->vertices();
  unordered_set<int> visited;

  unsigned int components = 0;

  for (auto node : nodes) {
    if (visited.find(node) == visited.end()) {
      // Not visited yet
      // Do BFS
      bfs(*g, node, visited);
      // All the components will be in visited
      components++;
    }
  }
  return components;
}

/**
 * Reads a csv file of the specified format in the assignment
 *
 * Loads a file using std::ifstream, reading line by line and getting substrings for each line to get the information
 * needed.
 *
 * @param filename The file that should be read
 * @return a pointer to heap allocated digraph object
 */
Digraph *read_city_graph_undirected(char filename[]) {
  // New graph in heap.
  auto graph = new Digraph();

  std::ifstream file(filename);
  std::string line;

  // repeat while not EOF
  while (std::getline(file, line)) {
    if (line[0] == 'V') {
      // Vertex
      // Find next comma in the line
      auto next_comma = line.find(',', 2);
      auto ID = std::stoi(line.substr(2, next_comma - 2));

      graph->addVertex(ID);
    } else if (line[0] == 'E') {
      auto first_comma = line.find(',', 2);
      auto first_vertex = std::stoi(line.substr(2, first_comma - 2));
      auto second_comma = line.find(',', first_comma + 1);
      // stoi uses lengths not positions
      auto second_vertex = std::stoi(line.substr(first_comma + 1, second_comma - first_comma - 1));

      // Undirected: Have to add both directions.
      graph->addEdge(first_vertex, second_vertex);
      graph->addEdge(second_vertex, first_vertex);
    }
  }

  return graph;
}

int main(int argc, char *argv[]) {
  int x, y, z, a;
  x = (y = 3, (z = ++y + 2) + 5);
  a = 249, 500;
  a = (x++, 500);
  x;
}
