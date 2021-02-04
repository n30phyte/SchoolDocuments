package com.n30phyte.trialbook;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.ContextMenu;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.text.SimpleDateFormat;
import java.util.List;

public class ExperimentLogAdapter extends RecyclerView.Adapter<ExperimentLogAdapter.ExperimentViewHolder> {

    private final List<Experiment> experiments;

    public ExperimentLogAdapter(List<Experiment> experimentList) {
        experiments = experimentList;
    }

    @NonNull
    @Override
    public ExperimentViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.experiment_log_item, parent, false);
        return new ExperimentViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final ExperimentViewHolder holder, int position) {
        holder.experiment = experiments.get(position);
        holder.experimentId = position;

        holder.descriptionTextView.setText(holder.experiment.getName());

        @SuppressLint("SimpleDateFormat") SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        String dateText = sdf.format(holder.experiment.getRecordedDate());
        holder.dateTextView.setText(dateText);
    }

    @Override
    public int getItemCount() {
        return experiments.size();
    }

    public class ExperimentViewHolder extends RecyclerView.ViewHolder implements View.OnCreateContextMenuListener {
        public final View view;
        public final TextView descriptionTextView;
        public final TextView dateTextView;

        public int experimentId;
        public Experiment experiment;

        public ExperimentViewHolder(View view) {
            super(view);

            this.view = view;
            descriptionTextView = view.findViewById(R.id.description);
            dateTextView = view.findViewById(R.id.date);

            view.setOnCreateContextMenuListener(this);
        }

        // Initially based off https://gist.github.com/gauravat16/e8e03496a4056829e65dede3c236da28
        // By Gaurav Sharma (gauravat16)
        @Override
        public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
            MenuItem edit = menu.add(Menu.NONE, 0, 0, "Edit");
            MenuItem delete = menu.add(Menu.NONE, 1, 1, "Delete");

            edit.setOnMenuItemClickListener(this::onMenuItemClick);
            delete.setOnMenuItemClickListener(this::onMenuItemClick);
        }

        private boolean onMenuItemClick(MenuItem item) {
            switch (item.getItemId()) {
                case 0:
                    // Edit button
                    break;
                case 1:
                    // Delete button
                    experiments.remove(experiment);
                    notifyItemRemoved(getAdapterPosition());
                    notifyItemRangeChanged(getAdapterPosition(), experiments.size());
                    break;
            }
            return true;
        }
    }

}