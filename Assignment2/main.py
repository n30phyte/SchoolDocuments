from typing import Optional

import matplotlib.pyplot as plt
import numpy as np


class PlotResults:
    """
    Class to plot the results.
    """

    def plot_results(self, data1, data2, label1, label2, filename):
        """
        This method receives two lists of data point (data1 and data2) and plots
        a scatter plot with the information. The lists store statistics about individual search
        problems such as the number of nodes a search algorithm needs to expand to solve the problem.

        The function assumes that data1 and data2 have the same size.

        label1 and label2 are the labels of the axes of the scatter plot.

        filename is the name of the file in which the plot will be saved.
        """
        _, ax = plt.subplots()
        ax.scatter(
            data1, data2, s=100, c="g", alpha=0.5, cmap=plt.cm.coolwarm, zorder=10
        )

        lims = [
            np.min([ax.get_xlim(), ax.get_ylim()]),  # min of both axes
            np.max([ax.get_xlim(), ax.get_ylim()]),  # max of both axes
        ]

        ax.plot(lims, lims, "k-", alpha=0.75, zorder=0)
        ax.set_aspect("equal")
        ax.set_xlim(lims)
        ax.set_ylim(lims)
        plt.xlabel(label1)
        plt.ylabel(label2)
        plt.grid()
        plt.savefig(filename)


class Grid:
    """
    Class to represent an assignment of values to the 81 variables defining a Sudoku puzzle.

    Variable _cells stores a matrix with 81 entries, one for each variable in the puzzle.
    Each entry of the matrix stores the domain of a variable. Initially, the domains of variables
    that need to have their values assigned are 123456789; the other domains are limited to the value
    initially assigned on the grid. Backtracking search and AC3 reduce the the domain of the variables
    as they proceed with search and inference.
    """

    _cells: list[list[str]]

    def __init__(self):
        self._cells = []
        self._complete_domain = "123456789"
        self._width = 9

    def copy(self):
        """
        Returns a copy of the grid.
        """
        copy_grid = Grid()
        copy_grid._cells = [row.copy() for row in self._cells]
        return copy_grid

    def get_cells(self):
        """
        Returns the matrix with the domains of all variables in the puzzle.
        """
        return self._cells

    def get_width(self):
        """
        Returns the width of the grid.
        """
        return self._width

    def read_file(self, string_puzzle):
        """
        Reads a Sudoku puzzle from string and initializes the matrix _cells.

        This is a valid input string:

        4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......

        This is translated into the following Sudoku grid:

        - - - - - - - - - - - - -
        | 4 . . | . . . | 8 . 5 |
        | . 3 . | . . . | . . . |
        | . . . | 7 . . | . . . |
        - - - - - - - - - - - - -
        | . 2 . | . . . | . 6 . |
        | . . . | . 8 . | 4 . . |
        | . . . | . 1 . | . . . |
        - - - - - - - - - - - - -
        | . . . | 6 . 3 | . 7 . |
        | 5 . . | 2 . . | . . . |
        | 1 . 4 | . . . | . . . |
        - - - - - - - - - - - - -
        """
        i = 0
        row = []
        for p in string_puzzle:
            if p == ".":
                row.append(self._complete_domain)
            else:
                row.append(p)

            i += 1

            if i % self._width == 0:
                self._cells.append(row)
                row = []

    def print(self):
        """
        Prints the grid on the screen. Example:

        - - - - - - - - - - - - -
        | 4 . . | . . . | 8 . 5 |
        | . 3 . | . . . | . . . |
        | . . . | 7 . . | . . . |
        - - - - - - - - - - - - -
        | . 2 . | . . . | . 6 . |
        | . . . | . 8 . | 4 . . |
        | . . . | . 1 . | . . . |
        - - - - - - - - - - - - -
        | . . . | 6 . 3 | . 7 . |
        | 5 . . | 2 . . | . . . |
        | 1 . 4 | . . . | . . . |
        - - - - - - - - - - - - -
        """
        for _ in range(self._width + 4):
            print("-", end=" ")
        print()

        for i in range(self._width):

            print("|", end=" ")

            for j in range(self._width):
                if len(self._cells[i][j]) == 1:
                    print(self._cells[i][j], end=" ")
                elif len(self._cells[i][j]) > 1:
                    print(".", end=" ")
                else:
                    print(";", end=" ")

                if (j + 1) % 3 == 0:
                    print("|", end=" ")
            print()

            if (i + 1) % 3 == 0:
                for _ in range(self._width + 4):
                    print("-", end=" ")
                print()
        print()

    def print_domains(self):
        """
        Print the domain of each variable for a given grid of the puzzle.
        """
        for row in self._cells:
            print(row)

    def is_solved(self):
        """
        Returns True if the puzzle is solved and False otherwise.
        """
        for i in range(self._width):
            for j in range(self._width):
                if len(self._cells[i][j]) != 1:
                    return False
        return True


class VarSelector:
    """
    Interface for selecting variables in a partial assignment.

    Extend this class when implementing a new heuristic for variable selection.
    """

    def select_variable(self, grid: Grid) -> tuple[int, int]:
        pass


class FirstAvailable(VarSelector):
    """
    Naïve method for selecting variables; simply returns the first variable encountered whose domain is larger than one.
    """

    def select_variable(self, grid: Grid) -> tuple[int, int]:
        # Implement here the first available heuristic
        for i in range(grid.get_width()):
            for j in range(grid.get_width()):
                if len(grid.get_cells()[i][j]) > 1:
                    return (i, j)
        return (-1, -1)


class MRV(VarSelector):
    """
    Implements the MRV heuristic, which returns one of the variables with smallest domain.
    """

    def select_variable(self, grid: Grid) -> tuple[int, int]:
        # Implement here the mrv heuristic
        pass


class AC3:
    """
    This class implements the methods needed to run AC3 on Sudoku.
    """

    def remove_domain_row(self, grid, row, column):
        """
        Given a matrix (grid) and a cell on the grid (row and column) whose domain is of size 1 (i.e., the variable has its
        value assigned), this method removes the value of (row, column) from all variables in the same row.
        """
        variables_assigned = []

        for j in range(grid.get_width()):
            if j != column:
                new_domain = grid.get_cells()[row][j].replace(
                    grid.get_cells()[row][column], ""
                )

                if len(new_domain) == 0:
                    return None, True

                if len(new_domain) == 1 and len(grid.get_cells()[row][j]) > 1:
                    variables_assigned.append((row, j))

                grid.get_cells()[row][j] = new_domain

        return variables_assigned, False

    def remove_domain_column(self, grid, row, column):
        """
        Given a matrix (grid) and a cell on the grid (row and column) whose domain is of size 1 (i.e., the variable has its
        value assigned), this method removes the value of (row, column) from all variables in the same column.
        """
        variables_assigned = []

        for j in range(grid.get_width()):
            if j != row:
                new_domain = grid.get_cells()[j][column].replace(
                    grid.get_cells()[row][column], ""
                )

                if len(new_domain) == 0:
                    return None, True

                if len(new_domain) == 1 and len(grid.get_cells()[j][column]) > 1:
                    variables_assigned.append((j, column))

                grid.get_cells()[j][column] = new_domain

        return variables_assigned, False

    def remove_domain_unit(self, grid, row, column):
        """
        Given a matrix (grid) and a cell on the grid (row and column) whose domain is of size 1 (i.e., the variable has its
        value assigned), this method removes the value of (row, column) from all variables in the same unit.
        """
        variables_assigned = []

        row_init = (row // 3) * 3
        column_init = (column // 3) * 3

        for i in range(row_init, row_init + 3):
            for j in range(column_init, column_init + 3):
                if i == row and j == column:
                    continue

                new_domain = grid.get_cells()[i][j].replace(
                    grid.get_cells()[row][column], ""
                )

                if len(new_domain) == 0:
                    return None, True

                if len(new_domain) == 1 and len(grid.get_cells()[i][j]) > 1:
                    variables_assigned.append((i, j))

                grid.get_cells()[i][j] = new_domain
        return variables_assigned, False

    def pre_process_consistency(self, grid):
        """
        This method enforces arc consistency for the initial grid of the puzzle.

        The method runs AC3 for the arcs involving the variables whose values are
        already assigned in the initial grid.
        """
        # Implement here the code for making the CSP arc consistent as a pre-processing step; this method should be called once before search
        pass

    def consistency(self, grid, Q):
        """
        This is a domain-specific implementation of AC3 for Sudoku.

        It keeps a set of variables to be processed (Q) which is provided as input to the method.
        Since this is a domain-specific implementation, we don't need to maintain a graph and a set
        of arcs in memory. We can store in Q the cells of the grid and, when processing a cell, we
        ensure arc consistency of all variables related to this cell by removing the value of
        cell from all variables in its column, row, and unit.

        For example, if the method is used as a preprocessing step, then Q is initialized with
        all cells that start with a number on the grid. This method ensures arc consistency by
        removing from the domain of all variables in the row, column, and unit the values of
        the cells given as input. Like the general implementation of AC3, the method adds to
        Q all variables that have their values assigned during the propagation of the contraints.

        The method returns True if AC3 detected that the problem can't be solved with the current
        partial assignment; the method returns False otherwise.
        """
        # Implement here the domain-dependent version of AC3.
        pass


class Backtracking:
    """
    Class that implements backtracking search for solving CSPs.
    """

    def check_consistency(
        self, grid: Grid, variable: tuple[int, int], domain: str
    ) -> bool:
        # Check column consistency
        for i in range(grid.get_width()):
            if i != variable[0]:
                new_domain = grid.get_cells()[i][variable[1]].replace(domain, "")

                if len(new_domain) == 0:
                    return False

        # Check row consistency
        for j in range(grid.get_width()):
            if j != variable[1]:
                new_domain = grid.get_cells()[variable[0]][j].replace(domain, "")

                if len(new_domain) == 0:
                    return False

        # Check unit consistency
        row_init = (variable[0] // 3) * 3
        column_init = (variable[1] // 3) * 3

        for i in range(row_init, row_init + 3):
            for j in range(column_init, column_init + 3):
                if (i, j) == variable:
                    continue

                new_domain = grid.get_cells()[i][j].replace(domain, "")

                if len(new_domain) == 0:
                    return False

        return True

    def search(self, grid: Grid, var_selector: VarSelector) -> Optional[Grid]:
        """
        Implements backtracking search with inference.
        """
        if grid.is_solved():
            return grid

        variable = var_selector.select_variable(grid)

        for value in grid.get_cells()[variable[0]][variable[1]]:
            if self.check_consistency(grid, variable, value):
                cloned_grid = grid.copy()
                cloned_grid.get_cells()[variable[0]][variable[1]] = value

                rb = Backtracking().search(cloned_grid, var_selector)

                if rb is not None:
                    return rb

        return None


if __name__ == "__main__":
    file = open("tutorial_problem.txt", "r")
    # file = open('top95.txt', 'r')
    problems = file.readlines()

    for p in problems:
        # Read problem from string
        g = Grid()
        g.read_file(p)

        print("Currently solving puzzle: ")
        g.print()
        print()

        solved = Backtracking().search(g, var_selector=FirstAvailable())

        solved.print()
