package com.n30phyte.trialbook;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

import androidx.constraintlayout.widget.ConstraintLayout;

import java.text.SimpleDateFormat;
import java.util.Date;

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
    private final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
    private int position;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_experiment_info);

        descriptionEditText = findViewById(R.id.et_experiment_description);
        dateEditText = findViewById(R.id.et_experiment_date);
        experimentTrialsTextView = findViewById(R.id.tv_trial_count);
        experimentPassRateTextView = findViewById(R.id.tv_pass_rate);
        experimentPassTextView = findViewById(R.id.tv_pass_count);
        experimentFailTextView = findViewById(R.id.tv_fail_count);

        ConstraintLayout experimentData = findViewById(R.id.layout_experiment_data);

        if (!getIntent().hasExtra(EXP_ARG)) {
            experiment = null;

            experimentData.setVisibility(View.GONE);
        } else {
            experiment = (Experiment) getIntent().getSerializableExtra(EXP_ARG);
            position = getIntent().getIntExtra(POS_ARG, -1);
            experimentData.setVisibility(View.VISIBLE);
        }

        Button saveButton = findViewById(R.id.btn_save);
        saveButton.setOnClickListener(this::onSave);

        Button passButton = findViewById(R.id.btn_experiment_pass);
        passButton.setOnClickListener(this::onPass);

        Button failButton = findViewById(R.id.btn_experiment_fail);
        failButton.setOnClickListener(this::onFail);

        updateViews();
    }

    private void updateViews() {
        if (experiment == null) {
            experimentTrialsTextView.setText("0");
            experimentPassRateTextView.setText("0");
            experimentPassTextView.setText("0");
            experimentFailTextView.setText("0");
        } else {
            descriptionEditText.setText(experiment.getDescription());
            String experimentDate = sdf.format(experiment.getRecordedDate());
            dateEditText.setText(experimentDate);

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
        // Save item

        String experimentDescription = descriptionEditText.getText().toString();

        if (experimentDescription.isEmpty()) {
            descriptionEditText.setError("Input a description");
            return;
        }

        if (dateEditText.getText().toString().isEmpty()) {
            dateEditText.setError("Input a date");
            return;
        }

        try {
            Date experimentDate = sdf.parse(dateEditText.getText().toString());
            Intent data = new Intent();

            if (experiment == null) {
                data.putExtra(EXP_ARG, new Experiment(experimentDescription, experimentDate));
            } else {
                experiment.setDescription(experimentDescription);
                experiment.setRecordedDate(experimentDate);
                data.putExtra(EXP_ARG, experiment);
                data.putExtra(POS_ARG, position);

            }
            setResult(RESULT_OK, data);
            finish();

        } catch (Exception e) {
            // Invalid date
            dateEditText.setError("Invalid Date Format");
        }
    }

}