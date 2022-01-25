package com.n30phyte.lonelytwitter;

import androidx.annotation.NonNull;

import java.util.Date;

public class SadMood extends Mood {

    public SadMood() {
        super();
    }

    public SadMood(Date date) {
        super(date);
    }

    @NonNull
    @Override
    public String toString() {
        return "Sad";
    }
}
