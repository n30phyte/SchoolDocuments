package com.n30phyte.lonelytwitter;

import java.util.Date;

public abstract class Tweet implements Tweetable {
    private Date date;
    private String message;

    public abstract boolean isImportant();

    public Tweet(String message) {
        this.date = new Date();
        this.message = message;
    }

    public Tweet(Date date, String message) {
        this.date = date;
        this.message = message;
    }

    @Override
    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    @Override
    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
