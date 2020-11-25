package ece325;

/**
 * Lab 4: Generics <br />
 * The {@code TrieNode} class <br />
*/
class TrieNode<V> {
	static final int ALPHABET_SIZE = 26;

	TrieNode<V>[] child = new TrieNode[ALPHABET_SIZE];
	boolean isEndOfWord;
	V value;
	
	TrieNode(){ 
		isEndOfWord = false;
		value = null; 
		for (int i = 0; i < ALPHABET_SIZE; i++) 
			child[i] = null; 
	}
	
	TrieNode(V val){ 
		isEndOfWord = true;
		value = val; 
		for (int i = 0; i < ALPHABET_SIZE; i++) 
			child[i] = null; 
	}
}

