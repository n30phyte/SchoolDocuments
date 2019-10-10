import bitio
import huffman
import pickle


def read_tree(tree_stream):
    """Read a description of a Huffman tree from the given compressed
    tree stream, and use the pickle module to construct the tree object.
    Then, return the root node of the tree itself.

    Args:
      tree_stream: The compressed stream to read the tree from.

    Returns:
      A Huffman tree root constructed according to the given description.
    """

    return pickle.load(tree_stream)


def decode_byte(tree, bitreader):
    """
    Reads bits from the bit reader and traverses the tree from
    the root to a leaf. Once a leaf is reached, bits are no longer read
    and the value of that leaf is returned.

    Args:
      bitreader: An instance of bitio.BitReader to read the tree from.
      tree: A Huffman tree.

    Returns:
      Next byte of the compressed bit stream.
    """

    # Iteration from recursion
    # Checks if the current part of the tree is a branch. Return value if not.
    while isinstance(tree, huffman.TreeBranch):
        # If bit = 0, go left. if bit = 1, go right.
        if bitreader.readbit():
            tree = tree.getRight()
        else:
            tree = tree.getLeft()

    return tree.getValue()


def decompress(compressed, uncompressed):
    """First, read a Huffman tree from the 'compressed' stream using your
    read_tree function. Then use that tree to decode the rest of the
    stream and write the resulting symbols to the 'uncompressed'
    stream.

    Args:
      compressed: A file stream from which compressed input is read.
      uncompressed: A writable file stream to which the uncompressed
          output is written.
    """

    huffman_root = read_tree(compressed)
    file_stream = bitio.BitReader(compressed)

    output = bytearray()

    # calls the lambda repeatedly until it returns None
    for decoded in iter(lambda: decode_byte(huffman_root, file_stream), None):
        output.append(decoded)

    uncompressed.write(output)


def write_tree(tree, tree_stream):
    """Write the specified Huffman tree to the given tree_stream
    using pickle.

    Args:
      tree: A Huffman tree.
      tree_stream: The binary file to write the tree to.
    """

    pickle.dump(tree, tree_stream)


def read_byte(bit_stream):
    """
    Short function to read the file, just to allow for use of iter(), decreasing clutter
    :param bit_stream: a BitReader object
    :return: None when EOF, the byte read otherwise.
    """
    try:
        return bit_stream.readbits(8)
    except EOFError:
        return None


def compress(tree, uncompressed, compressed):
    """First write the given tree to the stream 'compressed' using the
    write_tree function. Then use the same tree to encode the data
    from the input stream 'uncompressed' and write it to 'compressed'.
    If there are any partially-written bytes remaining at the end,
    write 0 bits to form a complete byte.

    Flush the BitWriter after writing the entire compressed file.

    Args:
      tree: A Huffman tree.
      uncompressed: A file stream from which you can read the input.
      compressed: A file stream that will receive the tree description
          and the coded input data.
    """

    write_tree(tree, compressed)
    encoding_table = huffman.make_encoding_table(tree)

    # Open a BitReader for uncompressed file
    in_stream = bitio.BitReader(uncompressed)

    output = []

    # Read the uncompressed file until EOF
    for uncompressed_byte in iter(lambda: read_byte(in_stream), None):
        compressed_byte = encoding_table[uncompressed_byte]
        output += list(compressed_byte)

    # Add EOF
    output += list(encoding_table[None])

    # Open BitWriter for compressed file
    out_stream = bitio.BitWriter(compressed)

    # Write out bits
    for bit in output:
        out_stream.writebit(bit)
    # Flush stream
    out_stream.flush()
