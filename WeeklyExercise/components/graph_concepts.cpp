// A template for the Graph Concepts Exercise in C++.
#include <queue>
#include <string>
#include <unordered_set>
#include <algorithm>
#include <fstream>
#include <iostream>
using namespace std;

#include "digraph.h"

void breadthFirst(Digraph *g, int thisnode, unordered_set<int> &seen) {
  //queue of nodes
  queue<int> list;

  //adds first node
  list.push(thisnode);

  //start search
  while (list.empty() == false) {
    //puts vertex at front of the list
    auto node = list.front();
    //removes latest element added to stack
    list.pop();

    for (auto i = g->neighbours(node); i != g->endIterator(node); i++) {
      //if next node cannot be found in seen
      if (seen.end() == seen.find(*i)) {
        //push element in list
        list.push(*i);
        //insert element in seen
        seen.insert(*i);
      }
    }
  }

}

int count_components(Digraph *g) {
  auto count = 0;
  //seen nodes
  unordered_set<int> seen;
  //list of nodes
  vector<int> nodes = g->vertices();

  //for element thisnode in nodes
  for (auto thisnode : nodes) {
    //if not found in seen
    if (seen.end() == seen.find(thisnode)) {
      breadthFirst(g, thisnode, seen);
      count++;
    }
  }
  return count;
}


Digraph* read_city_graph_undirected(char filename[]) {
  //new graph
  auto newGraph = new Digraph();
  ifstream file(filename);
  string str;

  while(getline(file, str)) {
    if (str[0] == 'E') {
      auto comma1 = str.find(',', 2);
      auto node1 = stoi(str.substr(2, comma1 - 2));
      auto comma2 = str.find(',', comma1 + 1);
      auto node2 = stoi(str.substr(comma1 + 1, comma2 - comma1 -1));
      newGraph->addEdge(node1, node2);
      newGraph->addEdge(node2, node1);
    }
    if (str[0] == 'V') {
      auto comma = str.find(',', 2);
      auto ID = stoi(str.substr(2, comma -2));
      newGraph->addVertex(ID);
    }

  }

return newGraph;
}


int main(int argc, char *argv[]) {
  auto newGraph = read_city_graph_undirected(argv[1]);
  cout << count_components(newGraph) << endl;
  delete newGraph;
  return 0;
}
