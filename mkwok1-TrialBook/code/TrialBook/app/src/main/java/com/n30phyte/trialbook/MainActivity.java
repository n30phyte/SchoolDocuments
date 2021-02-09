package com.n30phyte.trialbook;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.ListView;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

public class MainActivity extends AppCompatActivity {

    private ExperimentAdapter elAdapter;
    private ListView experimentList;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // Set up recyclerview for experiment list
        experimentList = findViewById(R.id.experiment_list);
        elAdapter = new ExperimentAdapter(this, ExperimentLog.getInstance());
        experimentList.setAdapter(elAdapter);

        experimentList.setOnItemClickListener((parent, view, position, id) -> onItemTouched(position));
    }

    public void fabTapped(View view) {
        Intent intent = new Intent(this, ExperimentInfoActivity.class);
        startActivityForResult(intent, 1);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == 1) {
            Experiment newExperiment = (Experiment) data.getSerializableExtra(ExperimentInfoActivity.EXP_ARG);

            elAdapter.add(newExperiment);
        } else if (requestCode == 2) {
            Experiment newExperiment = (Experiment) data.getSerializableExtra(ExperimentInfoActivity.EXP_ARG);
            int experimentId = data.getIntExtra(ExperimentInfoActivity.POS_ARG, -1);

            ExperimentLog.getInstance().set(experimentId, newExperiment);

            elAdapter.notifyDataSetChanged();
        }
    }

    public void onItemTouched(int editPosition) {
        Intent intent = new Intent(this, ExperimentInfoActivity.class);
        Bundle bundle = new Bundle();

        Experiment toEdit = elAdapter.getItem(editPosition);

        bundle.putInt(ExperimentInfoActivity.POS_ARG, editPosition);
        bundle.putSerializable(ExperimentInfoActivity.EXP_ARG, toEdit);
        intent.putExtras(bundle);

        startActivityForResult(intent, 2);
    }
}



/*

 */