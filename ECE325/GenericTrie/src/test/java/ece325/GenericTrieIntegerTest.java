package ece325;

import static org.junit.Assert.*;

import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Unit test for GenericTrie
 */
public class GenericTrieIntegerTest 
{

    static String[] words = {"ece", "lab", "java", "jar", "car", "cat", "care", "laboratory", "ebook"};
    static Integer[] values = {10, 20, 30, 35, 40, 42, 60, 102, 45}; 
    
    static GenericTrie<String, Integer> trie = new GenericTrie<String, Integer>();

    @BeforeClass
    public static void initTrie() 
    {
        for (int i = 0; i < words.length ; i++) 
			trie.insert(words[i], values[i]); 
    }

    @Test
    public void searchLab()
    {   
        Integer res = trie.search("lab");
        assertNotNull("'lab' should be in trie", res);  
        assertEquals(20, res, 0);
    }

    @Test
    public void searchJava()
    {   
        Integer res = trie.search("java");
        assertNotNull("'java' should be in trie", res);  
        assertEquals(30, res, 0);
    }

    @Test
    public void startWithFalse()
    {   
        assertFalse(trie.startWith("eced"));
    }

    @Test
    public void startWithTrue()
    {   
        assertTrue(trie.startWith("ca"));
    }
				
    @Test
    public void searchBook()
    {   
        Integer res = trie.search("book");
        assertNull("'book' is not in trie", res);
    }

    @Test
    public void removeInTrie()
    {   
        Integer res = trie.remove("jar");
        assertNotNull("'jar' should be in trie", res);
        assertEquals(35, res, 0); 
    }

    @Test
    public void removeNotInTrie()
    {   
        Integer res = trie.remove("capitan");
        assertNull("'capitan' is not in trie", res);
    }

}
