package com.n30phyte.lonelytwitter;

import androidx.annotation.NonNull;

import java.util.Date;

public class HappyMood extends Mood {

    public HappyMood() {
        super();
    }

    public HappyMood(Date date) {
        super(date);
    }

    @NonNull
    @Override
    public String toString() {
        return "Happy";
    }
}
