package com.n30phyte.lonelytwitter;

import androidx.annotation.NonNull;

import java.util.Date;

public abstract class Mood {
    private Date date;

    @NonNull
    public abstract String toString();

    Mood() {
        this.date = new Date();
    }

    Mood(Date date) {
        this.date = date;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

}
