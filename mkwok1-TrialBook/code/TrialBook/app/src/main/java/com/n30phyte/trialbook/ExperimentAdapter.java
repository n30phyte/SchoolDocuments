package com.n30phyte.trialbook;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.text.SimpleDateFormat;
import java.util.ArrayList;

public class ExperimentAdapter extends ArrayAdapter<Experiment> {

    private final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

    private final ArrayList<Experiment> experiments;
    private final Context context;
    private OnAdapterTouchedListener listener;

    public ExperimentAdapter(Context context, ArrayList<Experiment> experimentList) {
        super(context, 0, experimentList);
        this.experiments = experimentList;
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

        TextView experimentDescription = view.findViewById(R.id.tv_experiment_description);
        TextView experimentDate = view.findViewById(R.id.tv_experiment_date);

        TextView experimentTrials = view.findViewById(R.id.tv_experiment_trials);
        TextView experimentSuccessRate = view.findViewById(R.id.tv_experiment_success_rate);

        experimentDescription.setText(experiment.getDescription());
        experimentDate.setText(sdf.format(experiment.getRecordedDate()));

        experimentTrials.setText(String.format("Pass: %s", experiment.getTrials()));

        float successRate = ((float) experiment.getPass() / experiment.getTrials()) * 100;
        experimentSuccessRate.setText(String.format("%s%%", successRate));

        return view;
    }

    public interface OnAdapterTouchedListener {
        void onItemTouched(Experiment toEdit);
    }
}