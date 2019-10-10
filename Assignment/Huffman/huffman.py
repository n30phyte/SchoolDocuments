class TreeLeaf:
    """
    Leaf node of a Huffman tree. Stores the value.

    Should store an 8-bit integer to represent a single byte, or None
    to indicate the special "end of file" character.
    """

    def __init__(self, uncompressed_byte):
        self.__value = uncompressed_byte

    def getValue(self):
        return self.__value

    def __str__(self):
        return "Leaf storing " + str(self.__value)


class TreeBranch:
    """
    Simple representation of a subtree/tree of a Huffman tree.
    Just stores the two children.
    """

    def __init__(self, lchild, rchild):
        self.__left = lchild
        self.__right = rchild

    def getLeft(self):
        return self.__left

    def getRight(self):
        return self.__right

    def __str__(self):
        return "(" + str(self.__left) + " <- branch root -> " + str(self.__right) + ")"


def custom_min(trees):
    """ Takes a list of tuples called trees, finds the smallest 
    item and removes it from the list. Both the smallest item and
    new list are returned.

    Each item in trees is a tuple of (symbol, frequency)
    """

    if len(trees) == 0:
        raise ValueError("The list passed as input was empty.")

    # default to the first item
    min_item = trees[0]
    min_index = 0

    for i in range(len(trees)):
        # if this item has a smaller frequency
        if trees[i][1] < min_item[1]:
            min_item = trees[i]
            min_index = i

    del trees[min_index]

    return min_item[0], min_item[1], trees


def make_tree(freq_table):
    """
    Constructs and returns the Huffman tree from the given frequency table.
    """

    trees = []
    trees.append((TreeLeaf(None), 1))

    for (symbol, freq) in freq_table.items():
        trees.append((TreeLeaf(symbol), freq))

    while len(trees) > 1:
        right, rfreq, trees = custom_min(trees)
        left, lfreq, trees = custom_min(trees)
        trees.append((TreeBranch(left, right), lfreq+rfreq))

    return trees[0][0]


def make_encoding_table(huffman_tree):
    """
    Given a Huffman tree, will make the encoding table mapping each
    byte (leaf node) to its corresponding bit sequence in the tree.
    """
    encoding_table = {}
    preorder(huffman_tree, encoding_table, ())
    return encoding_table


def preorder(tree, table, path):
    """
    Traces out all paths in the Huffman tree and adds each
    corresponding leaf value and its associated path to the table.
    """
    if isinstance(tree, TreeLeaf):  # base case
        # note, if this is the special end file entry then
        # it stores table[None] = path
        table[tree.getValue()] = path
    elif isinstance(tree, TreeBranch):
        # the trailing comma (,) means this is properly interpreted
        # as a tuple and not just a single boolean value
        preorder(tree.getLeft(), table, path + (False, ))
        preorder(tree.getRight(), table, path + (True, ))
    else:
        raise TypeError('{} is not a tree type'.format(type(tree)))


def make_freq_table(stream):
    """
    Given an input stream, will construct a frequency table
    (i.e. mapping of each byte to the number of times it occurs in the stream).

    The frequency table is actually a dictionary.
    """
    freq_dict = {}
    buff = bytearray(512)
    end_of_stream = False
    while not end_of_stream:
        # read bytes into a pre-allocated bytes-like object (bytearray)
        # and return number of bytes read
        count = stream.readinto(buff)

        for single_byte in buff:
            if single_byte not in freq_dict:
                freq_dict[single_byte] = 1
            else:
                freq_dict[single_byte] += 1

        if count < len(buff):  # end of stream
            end_of_stream = True

    return freq_dict
