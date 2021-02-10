package com.n30phyte.trialbook;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

import androidx.constraintlayout.widget.ConstraintLayout;

import java.text.ParseException;

public class ExperimentInfoActivity extends Activity {

    public static final String EXP_ARG = "EXPERIMENT_OBJECT";
    public static final String POS_ARG = "EXPERIMENT_POSITION";
    EditText descriptionEditText;
    EditText dateEditText;
    TextView experimentTrialsTextView;
    TextView experimentPassRateTextView;
    TextView experimentPassTextView;
    TextView experimentFailTextView;
    private Experiment experiment;
    private int position;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_experiment_info);

        descriptionEditText = findViewById(R.id.et_info_description);
        dateEditText = findViewById(R.id.et_info_date);
        experimentTrialsTextView = findViewById(R.id.tv_info_trials);
        experimentPassRateTextView = findViewById(R.id.tv_info_rate);
        experimentPassTextView = findViewById(R.id.tv_info_pass);
        experimentFailTextView = findViewById(R.id.tv_info_fail);

        ConstraintLayout experimentData = findViewById(R.id.cl_info_data);

        if (!getIntent().hasExtra(EXP_ARG)) {
            // New experiment, hide trial data view.
            experiment = null;

            experimentData.setVisibility(View.GONE);
        } else {
            // Existing experiment, show trial data view.
            experiment = (Experiment) getIntent().getParcelableExtra(EXP_ARG);
            position = getIntent().getIntExtra(POS_ARG, -1);
            experimentData.setVisibility(View.VISIBLE);
        }

        Button saveButton = findViewById(R.id.btn_info_save);
        saveButton.setOnClickListener(this::onSave);

        Button passButton = findViewById(R.id.btn_info_pass);
        passButton.setOnClickListener(this::onPass);

        Button failButton = findViewById(R.id.btn_info_fail);
        failButton.setOnClickListener(this::onFail);

        updateViews();
    }

    /**
     * Sets up all the TextViews in the activity to show the stored data.
     */
    private void updateViews() {
        if (experiment != null) {
            descriptionEditText.setText(experiment.getDescription());
            dateEditText.setText(experiment.getDateAsString());

            experimentTrialsTextView.setText(String.valueOf(experiment.getTrials()));

            float successRate = ((float) experiment.getPass() / experiment.getTrials()) * 100;

            experimentPassRateTextView.setText(String.format("%s%%", successRate));
            experimentPassTextView.setText(String.valueOf(experiment.getPass()));
            experimentFailTextView.setText(String.valueOf(experiment.getTrials() - experiment.getPass()));
        }
    }

    private void onPass(View v) {
        experiment.incrementPass();
        experiment.incrementTrials();
        updateViews();
    }

    private void onFail(View v) {
        experiment.incrementTrials();
        updateViews();
    }

    private void onSave(View v) {
        String experimentDescription = descriptionEditText.getText().toString();
        String dateString = dateEditText.getText().toString();

        if (experimentDescription.isEmpty()) {
            descriptionEditText.setError("Input a description");
            return;
        }

        if (dateString.isEmpty()) {
            dateEditText.setError("Input a date");
            return;
        }

        try {
            Intent data = new Intent();

            if (experiment == null) {
                data.putExtra(EXP_ARG, new Experiment(experimentDescription, dateString));
            } else {
                experiment.setDescription(experimentDescription);
                experiment.setDateFromString(dateString);
                data.putExtra(EXP_ARG, experiment);
                data.putExtra(POS_ARG, position);

            }

            setResult(RESULT_OK, data);
            finish();

        } catch (ParseException e) {
            // Invalid date
            dateEditText.setError("Invalid Date Format");
        }
    }

}