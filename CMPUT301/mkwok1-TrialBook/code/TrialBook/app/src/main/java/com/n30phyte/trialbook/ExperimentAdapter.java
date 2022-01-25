package com.n30phyte.trialbook;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.ArrayList;

/**
 * Adapter for ListView to show Experiments as entries in the app.
 */
public class ExperimentAdapter extends ArrayAdapter<Experiment> {

    private final ArrayList<Experiment> experiments;
    private final Context context;

    public ExperimentAdapter(Context context) {
        super(context, 0, ExperimentLog.getInstance());
        this.experiments = ExperimentLog.getInstance();
        this.context = context;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View view = convertView;

        if (view == null) {
            view = LayoutInflater.from(context).inflate(R.layout.item_experiment, parent, false);
        }

        Experiment experiment = experiments.get(position);

        // Grab the views
        TextView tvItemDescription = view.findViewById(R.id.tv_item_description);
        TextView tvItemDate = view.findViewById(R.id.tv_item_date);
        TextView tvItemTrials = view.findViewById(R.id.tv_item_trials);
        TextView tbItemSuccessRate = view.findViewById(R.id.tv_item_rate);

        // Set the data up inside the views
        tvItemDescription.setText(experiment.getDescription());
        tvItemDate.setText(experiment.getDateAsString());
        tvItemTrials.setText(String.format("Trials: %s", experiment.getTrials()));

        float successRate = ((float) experiment.getPass() / experiment.getTrials()) * 100;
        tbItemSuccessRate.setText(String.format("%s%%", successRate));

        return view;
    }
}