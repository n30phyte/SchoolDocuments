package com.n30phyte.trialbook;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.RecyclerView;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;

public class MainActivity extends AppCompatActivity {

    private ExperimentLogAdapter elAdapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // Set up recyclerview for experiment list
        RecyclerView rv = (RecyclerView) findViewById(R.id.experimentList);

        elAdapter = new ExperimentLogAdapter(ExperimentLog.getInstance());

        rv.setAdapter(elAdapter);
    }

    public void fabTapped(View view) {
        // Don't put anything extra to show we want to make a new experiment.
        Intent intent = new Intent(this, ExperimentActivity.class);
        startActivity(intent);
    }
}