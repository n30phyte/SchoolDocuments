package com.n30phyte.lonelytwitter;

import java.util.Date;

public class ImportantTweet extends Tweet {
    public ImportantTweet(String message) {
        super(message);
    }

    public ImportantTweet(Date date, String message) {
        super(date, message);
    }

    @Override
    public boolean isImportant() {
        return true;
    }
}
