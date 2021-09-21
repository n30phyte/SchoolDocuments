from typing import Dict, List
import heapq


class State:
    """
    Class to represent a state on grid-based pathfinding problems. The class contains two static variables:
    map_width and map_height containing the width and height of the map. Although these variables are properties
    of the map and not of the state, they are used to compute the hash value of the state, which is used
    in the CLOSED list.

    Each state has the values of x, y, g, h, and cost. The cost is used as the criterion for sorting the nodes
    in the OPEN list for both Dijkstra's algorithm and A*. For Dijkstra the cost should be the g-value, while
    for A* the cost should be the f-value of the node.
    """

    map_width = 0
    map_height = 0

    def __init__(self, x: int, y: int):
        """
        Constructor - requires the values of x and y of the state. All the other variables are
        initialized with the value of 0.
        """
        self._x = x
        self._y = y
        self._g = 0
        self._h = 0
        self._cost = 0

    def __repr__(self):
        """
        This method is invoked when we call a print instruction with a state. It will print [x, y],
        where x and y are the coordinates of the state on the map.
        """
        state_str = "[" + str(self._x) + ", " + str(self._y) + "]"
        return state_str

    def __lt__(self, other):
        """
        Less-than operator; used to sort the nodes in the OPEN list
        """
        return self._cost < other._cost

    def state_hash(self):
        """
        Given a state (x, y), this method returns the value of x * map_width + y. This is a perfect
        hash function for the problem (i.e., no two states will have the same hash value). This function
        is used to implement the CLOSED list of the algorithms.
        """
        return self._y * State.map_width + self._x

    def __eq__(self, other):
        """
        Method that is invoked if we use the operator == for states. It returns True if self and other
        represent the same state; it returns False otherwise.
        """
        return self._x == other._x and self._y == other._y

    def get_x(self) -> int:
        """
        Returns the x coordinate of the state
        """
        return self._x

    def get_y(self) -> int:
        """
        Returns the y coordinate of the state
        """
        return self._y

    def get_g(self) -> float:
        """
        Returns the g-value of the state
        """
        return self._g

    def get_h(self) -> float:
        """
        Returns the h-value of the state
        """
        return self._h

    def get_cost(self) -> float:
        """
        Returns the cost of the state (g for Dijkstra's and f for A*)
        """
        return self._cost

    def set_g(self, cost: float):
        """
        Sets the g-value of the state
        """
        self._g = cost

    def set_h(self, h: float):
        """
        Sets the h-value of the state
        """
        self._h = h

    def set_cost(self, cost: float):
        """
        Sets the cost of a state (g for Dijkstra's and f for A*)
        """
        self._cost = cost


class Search:
    """
    Interface for a search algorithm. It contains an OPEN list and a CLOSED list.

    The OPEN list is implemented with a heap, which can be done with the library heapq
    (https://docs.python.org/3/library/heapq.html).

    The CLOSED list is implemented as a dictionary where the state hash value is used as key.
    """

    def __init__(self, gridded_map):
        self.map = gridded_map
        self.OPEN: List[State] = []
        self.CLOSED: Dict[int, State] = {}

    def search(self, start: State, goal: State):
        """
        Search method that needs to be implemented (either Dijkstra or A*).
        """
        raise NotImplementedError()


class Dijkstra(Search):
    def search(self, start: State, goal: State):
        """
        Disjkstra's Algorithm: receives a start state and a goal state as input. It returns the
        cost of a path between start and goal and the number of nodes expanded.

        If a solution isn't found, it returns -1 for the cost.
        """

        self.OPEN = []
        self.CLOSED = {}

        expand_count = 0

        start.set_cost(0)
        heapq.heappush(self.OPEN, start)
        self.CLOSED[start.state_hash()] = start

        while self.OPEN:
            node = heapq.heappop(self.OPEN)

            expand_count += 1

            if node == goal:
                return node.get_cost(), expand_count

            for new_node in self.map.successors(node):
                node_cost: float = node.get_cost() + self.map.cost(
                    new_node.get_x() - node.get_x(),
                    new_node.get_y() - node.get_y(),
                )

                new_hash = new_node.state_hash()

                if new_hash not in self.CLOSED:
                    new_node.set_cost(node_cost)
                    heapq.heappush(self.OPEN, new_node)
                    self.CLOSED[new_node.state_hash()] = new_node
                else:
                    existing_node = self.CLOSED[new_hash]

                    if node_cost < existing_node.get_cost():
                        existing_node.set_cost(node_cost)
                        heapq.heapify(self.OPEN)

        return -1, expand_count


class AStar(Search):
    def h_value(self, state: State):
        deltaX = abs(self.goal.get_x() - state.get_x())
        deltaY = abs(self.goal.get_y() - state.get_y())

        return max(deltaX, deltaY) + (0.5 * min(deltaX, deltaY))

    def update_cost(self, state: State, g_val: float, h_val: float):
        state.set_g(g_val)
        state.set_h(h_val)
        state.set_cost(g_val + h_val)

    def search(self, start: State, goal: State):
        """
        A* Algorithm: receives a start state and a goal state as input. It returns the
        cost of a path between start and goal and the number of nodes expanded.

        If a solution isn't found, it returns -1 for the cost.
        """
        self.goal = goal

        self.OPEN = []
        self.CLOSED = {}

        expand_count = 0

        start.set_g(0)
        start.set_h(self.h_value(start))
        start.set_cost(self.h_value(start))

        heapq.heappush(self.OPEN, start)
        self.CLOSED[start.state_hash()] = start

        while self.OPEN:
            node = heapq.heappop(self.OPEN)

            expand_count += 1

            if node == goal:
                return node.get_g(), expand_count

            for new_node in self.map.successors(node):
                node_g: float = node.get_g() + self.map.cost(
                    new_node.get_x() - node.get_x(),
                    new_node.get_y() - node.get_y(),
                )

                node_h = self.h_value(new_node)

                node_cost = node_g + node_h

                new_hash = new_node.state_hash()

                if new_hash not in self.CLOSED:
                    new_node.set_g(node_g)
                    new_node.set_h(node_h)
                    new_node.set_cost(node_cost)

                    heapq.heappush(self.OPEN, new_node)
                    self.CLOSED[new_node.state_hash()] = new_node
                else:
                    existing_node = self.CLOSED[new_hash]

                    if node_cost < existing_node.get_cost():
                        existing_node.set_g(node_g)
                        existing_node.set_h(node_h)
                        existing_node.set_cost(node_cost)

                        heapq.heapify(self.OPEN)

        return -1, expand_count
