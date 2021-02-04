package com.n30phyte.trialbook;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;

import java.text.SimpleDateFormat;
import java.util.Date;

public class ExperimentActivity extends AppCompatActivity {

    public static final String PARAM_EXPERIMENT_ID = "existingExperimentId";

    private boolean isNewExperiment;
    private Experiment experiment;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_experiment);

        Intent intent = getIntent();
        int experimentId = intent.getIntExtra(PARAM_EXPERIMENT_ID, -1);

        if (experimentId == -1) {
            isNewExperiment = true;
        } else {
            isNewExperiment = false;
            experiment = ExperimentLog.getInstance().get(experimentId);
        }

        if (!isNewExperiment) {
            TextView descriptionTextView = findViewById(R.id.editTextExperimentDescription);
            TextView dateTextView = findViewById(R.id.editTextDate);

            descriptionTextView.setText(experiment.getName());

            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            String experimentDate = sdf.format(experiment.getRecordedDate());

            dateTextView.setText(experimentDate);
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.menu_experiment, menu);
        return true;
    }


    public void onSaveExperiment(View view) {
        // Grab text from EditText
        EditText etExperimentDescription = findViewById(R.id.editTextExperimentDescription);
        EditText etExperimentDate = findViewById(R.id.editTextDate);

        String experimentDescription = etExperimentDescription.getText().toString();

        try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            Date experimentDate = sdf.parse(etExperimentDate.getText().toString());

            if (isNewExperiment) {
                ExperimentLog.addExperiment(new Experiment(experimentDescription, experimentDate));
            } else {
                experiment.setName(experimentDescription);
                experiment.setRecordedDate(experimentDate);
            }

            finish();
        } catch (Exception e) {
            // Invalid date
            etExperimentDate.setError("Invalid Date Format");
        }
    }

}