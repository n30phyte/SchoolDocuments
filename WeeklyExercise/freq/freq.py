# ---------------------------------------------------
# Name: Michael Kwok
# ID: 1548454
# CMPUT 274, Fall 2019
#
# Weekly Exercise 3: Frequency Counter
# ---------------------------------------------------

import sys


def save_frequency(count_table, input_file):
    """
    Calculates and saves a relative frequency table from the inputs provided.

    :param count_table: A dictionary with the word as the Key, and occurrence as Value
    :param input_file: The name of the file being read. This is used to name the file being written to.
    :return: None
    """
    # Opens new file to output to
    with open(f"{input_file}.out", "w") as text:
        # Total sum of every word's occurrence in the file.
        totalCount = sum(count_table.values())

        # Loop through each key and corresponding value in the dictionary
        for word, count in count_table.items():
            # Output the word, the count and the relative frequency of the word.
            text.write(f"{word} {count} {round(count / totalCount, 3)}\n")


# Parse file into dictionary
def parse_file(input_file):
    """
    Parses a file, and returns a dictionary

    :param input_file: The name of the file being read.
    :return: A python dict() with the word as the Key, and occurrence as Value
    """
    # Automatically close the file after being used
    with open(input_file) as text:
        # Read file and split each word into an element in a list
        data = text.read().split()

        # Sort the list
        # Python sort automatically does lexical sorting
        data.sort()

        # For each word, use as Dictionary key and count the occurrences of the word and use as value
        frequency_table = {word: data.count(word) for word in data}

        # Return the frequency table
        return frequency_table


if __name__ == "__main__":

    if len(sys.argv) > 2:
        print("Too many command-line arguments")
        print("Usage: python3 freq.py <filename>")
    elif len(sys.argv) < 2:
        print("Too few command-line arguments")
        print("Usage: python3 freq.py <filename>")
    else:
        filename = sys.argv[1]

        frequencyTable = parse_file(filename)

        save_frequency(frequencyTable, filename)

    pass
