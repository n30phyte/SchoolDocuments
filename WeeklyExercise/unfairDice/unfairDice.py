# ---------------------------------------------------
# Name: Michael Kwok
# ID: 1548454
# CMPUT 274, Fall 2019
#
# Weekly Exercise 2: Unfair Dice
# ---------------------------------------------------

import random


def biased_rolls(prob_list, s, n):
    """ Simulate n rolls of a biased m-sided die and return
    a list containing the results. 

    Arguments:
        prob_list: a list of the probabilities of rolling the 
                   number on each side of the m-sided die. The list  
                   will always have the length m (m >= 2), where m is 
                   the number of sides numbered 1 to m. Therefore,  
                   for example, the probability stored at index 0 in 
                   the list is the probability of rolling a 1 on
                   the m-sided die.
        s: the seed to use when initializing the PRNG
        n: the number of rolls to return

    Return:
        rolls: a list (of length n) containing each of the n rolls of the 
               biased die, in the order they were generated.
    """

    # List of all rolls
    rolls = list()

    random.seed(s)

    for _ in range(n):
        value = random.random()
        dieFace = 0

        for probability in prob_list:
            if value > 0:
                value = value - probability
                dieFace = dieFace + 1

        rolls.append(dieFace)

    # return the resulting rolls
    return rolls


def draw_histogram(m, rolls, width):
    """ Draws a frequency histogram of the rolls of an m-sided die
    mapped to a fixed width.

    Arguments: 
        m (int): the number of sides on the die
        rolls (list): the list of rolls generated by the biased die
        width (int): the fixed width of the histogram, in characters
                     (this is the length of the longest bar in the 
                     histogram, to maximize space in the chart)

    Returns:
        None (but prints the histogram to standard output)
    """

    # Required start
    print(f"Frequency Histogram: {m}-sided Die")

    # Count the occurrence of each dice face.
    counts = [rolls.count(dice) for dice in range(1, m + 1)]

    # Find the largest number of occurrences to scale graph with
    largest_count = max(counts)
    unit_size = largest_count / width

    # Go through each face and print out the bar
    for dice in range(1, m + 1):
        # Calculate the height of the asterisk bar
        bar_height = round(counts[dice - 1] / unit_size)
        # Generate bar
        bar = ["*"] * bar_height

        # Print out bar
        # ''.join(bar) converts the asterisk list into a string of asterisks
        # .<{width} Comes in 3 parts.
        # the '<' pushes the bar to the left
        # the '.' specifies the padding character
        # '{width}' specifies the width of the block;

        print(f"{dice}:{''.join(bar):.<{width}}")

    pass


if __name__ == "__main__":
    # Any code indented under this line will only be run
    # when the program is called directly from the terminal
    # using "python3 unfairDice.py". This can be useful for
    # testing your implementations.

    rolls = biased_rolls([1 / 3, 1 / 3, 1 / 3], 2 ** 32 - 1, 5000000)
    # draw_histogram(3, rolls, 1000)

    # draw_histogram(3, rolls, 100)

    pass
