from search.algorithms import Dijkstra, AStar, State
from search.map import Map
import time
import getopt
import sys

def main():
    """
    Function for testing your A* and Dijkstra's implementation. There is no need to edit this file.
    Run it with a -help option to see the options available. 
    """
    optlist, _ = getopt.getopt(sys.argv[1:], 'h:m:r:', ['testinstances', 'plots', 'help'])

    plots = False
    for o, a in optlist:
        if o in ("-help"):
            print("Examples of Usage:")
            print("Solve set of test instances: main.py --testinstances")
            print("Solve set of test instances and generate plots: main.py --testinstances --plots")
            exit()
        elif o in ("--plots"):
            plots = True
        elif o in ("--testinstances"):
            test_instances = "test-instances/testinstances.txt"
                              
    gridded_map = Map("dao-map/brc000d.map")
    dijkstra = Dijkstra(gridded_map)
    astar = AStar(gridded_map)
    
    nodes_expanded_dijkstra = []
    runtime_dijkstra = []
    solution_cost_dijkstra = []
    
    nodes_expanded_astar = []
    runtime_astar = []
    solution_cost_astar = []
    
    start_states = []
    goal_states = []
    solution_costs = []
       
    file = open(test_instances, "r")
    for instance_string in file:
        list_instance = instance_string.split(",")
        start_states.append(State(int(list_instance[0]), int(list_instance[1])))
        goal_states.append(State(int(list_instance[2]), int(list_instance[3])))
        
        solution_costs.append(float(list_instance[4]))
    file.close()
        
    for i in range(0, len(start_states)):    
        start = start_states[i]
        goal = goal_states[i]
    
        beg = time.time()
        cost, expanded_diskstra = dijkstra.search(start, goal)
        end = time.time()
        time_dijkstra = end - beg
        nodes_expanded_dijkstra.append(expanded_diskstra)
        solution_cost_dijkstra.append(cost)
        runtime_dijkstra.append(time_dijkstra)

        if cost != solution_costs[i]:
            print("There is a mismatch in the solution cost found by Dijkstra and what was expected for the problem:")
            print("Start state: ", start)
            print("Goal state: ", goal)
            print("Solution cost encountered: ", cost)
            print("Solution cost expected: ", solution_costs[i])
            print()

        beg = time.time()
        cost, expanded_astar = astar.search(start, goal)
        end = time.time()
        time_astar = end - beg
        nodes_expanded_astar.append(expanded_astar)
        solution_cost_astar.append(cost)
        runtime_astar.append(time_astar)
        
        if cost != solution_costs[i]:
            print("There is a mismatch in the solution cost found by A* and what was expected for the problem:")
            print("Start state: ", start)
            print("Goal state: ", goal)
            print("Solution cost encountered: ", cost)
            print("Solution cost expected: ", solution_costs[i])
            print()
    
    if plots:
        from search.plot_results import PlotResults
        plotter = PlotResults()
        plotter.plot_results(runtime_astar, runtime_dijkstra, "Running Time in Seconds (A*)", "Running Time in Seconds (Dijkstra)", "running_time")
        plotter.plot_results(nodes_expanded_astar, nodes_expanded_dijkstra, "Nodes Expanded (A*)", "Nodes Expanded (Dijkstra)", "nodes_expanded")
        plotter.plot_results(solution_cost_astar, solution_cost_dijkstra, "Solution Cost (A*)", "Solution Cost (Dijkstra)", "solution_cost")

if __name__ == "__main__":
    main()