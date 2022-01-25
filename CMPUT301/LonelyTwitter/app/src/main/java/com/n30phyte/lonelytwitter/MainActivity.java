package com.n30phyte.lonelytwitter;

import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;

import java.util.ArrayList;
import java.util.Date;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        ArrayList<Tweet> tweetList = new ArrayList<Tweet>();
        tweetList.add(new ImportantTweet("Important"));
        tweetList.add(new NormalTweet("Not"));

        ArrayList<Mood> moods = new ArrayList<>();
        moods.add(new SadMood(new Date()));
    }
}