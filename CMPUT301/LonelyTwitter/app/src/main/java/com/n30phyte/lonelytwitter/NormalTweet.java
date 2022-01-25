package com.n30phyte.lonelytwitter;

import java.util.Date;

public class NormalTweet extends Tweet {
    public NormalTweet(String message) {
        super(message);
    }

    public NormalTweet(Date date, String message) {
        super(date, message);
    }

    @Override
    public boolean isImportant() {
        return false;
    }
}
