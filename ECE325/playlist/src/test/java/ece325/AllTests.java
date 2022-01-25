package ece325;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

    public static Test suite() {
        TestSuite suite = new TestSuite("Test for default package");

        suite.addTestSuite(SongTest.class);
        suite.addTestSuite(PlaylistTest.class);

        return suite;
    }

}
