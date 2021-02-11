package com.n30phyte.trialbook;

import android.content.Intent;
import android.os.Bundle;
import android.view.ContextMenu;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

public class MainActivity extends AppCompatActivity implements View.OnCreateContextMenuListener {

    private static final int MENU_DELETE_ENTRY = 0;

    private static final int NEW_EXPERIMENT_ACTIVITY = 0;
    private static final int EDIT_EXPERIMENT_ACTIVITY = 1;

    private ExperimentAdapter elAdapter;
    private ListView experimentList;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // Set up adapter
        elAdapter = new ExperimentAdapter(this);

        // Set up ListView
        experimentList = findViewById(R.id.lv_main_experiment);
        experimentList.setAdapter(elAdapter);
        experimentList.setOnItemClickListener(this::onItemTouched);

        // For deletion
        registerForContextMenu(experimentList);

        // Add onClickListener to FAB
        FloatingActionButton fabAdd = findViewById(R.id.fab_main_add);
        fabAdd.setOnClickListener(this::fabTapped);
    }

    /**
     * This handles returning to this activity from the experiment editing one.
     *
     * @param requestCode The pre-specified request code (edit mode or add mode).
     * @param resultCode  The result from the swapped activity.
     * @param data        The data returned from the activity, if available.
     */
    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        // Only handle things if info activity completed successfully.
        if (resultCode == RESULT_OK) {
            Experiment newExperiment = (Experiment) data.getParcelableExtra(ExperimentInfoActivity.EXP_ARG);

            if (requestCode == NEW_EXPERIMENT_ACTIVITY) {
                ExperimentLog.addExperiment(newExperiment);
                elAdapter.notifyDataSetChanged();
            } else if (requestCode == EDIT_EXPERIMENT_ACTIVITY) {
                int experimentId = data.getIntExtra(ExperimentInfoActivity.POS_ARG, -1);
                ExperimentLog.setExperiment(experimentId, newExperiment);
                elAdapter.notifyDataSetChanged();
            }
        }
    }

    /**
     * Method called when the Floating Action Button to add a new entry is tapped
     */
    private void fabTapped(View view) {
        Intent intent = new Intent(this, ExperimentInfoActivity.class);
        startActivityForResult(intent, NEW_EXPERIMENT_ACTIVITY);
    }

    /**
     * Method called when an item in the ListView is tapped.
     */
    private void onItemTouched(AdapterView<?> parent, View view, int position, long id) {
        Intent intent = new Intent(this, ExperimentInfoActivity.class);
        Bundle bundle = new Bundle();

        Experiment toEdit = elAdapter.getItem(position);

        bundle.putInt(ExperimentInfoActivity.POS_ARG, position);
        bundle.putParcelable(ExperimentInfoActivity.EXP_ARG, toEdit);
        intent.putExtras(bundle);

        startActivityForResult(intent, EDIT_EXPERIMENT_ACTIVITY);
    }

    /*
     * Following two methods are implemented based on answer by Kamil Seweryn on StackOverflow
     * From: https://stackoverflow.com/a/21959658
     * Author: https://stackoverflow.com/users/1598308/kamil-seweryn
     * License: CC BY-SA 3.0
     * Edit date: 2014-02-22
     */

    /**
     * Method is called when setting up the context menu display, i.e. when the item is long pressed
     */
    @Override
    public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
        if (v.getId() == R.id.lv_main_experiment) {
            super.onCreateContextMenu(menu, v, menuInfo);
            AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) menuInfo;

            menu.add(Menu.NONE, MENU_DELETE_ENTRY, Menu.NONE, "Delete");
        }
    }

    /**
     * Method is called when a menu entry is selected.
     */
    @Override
    public boolean onContextItemSelected(MenuItem item) {
        if (item.getItemId() == MENU_DELETE_ENTRY) {
            AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) item.getMenuInfo();

            elAdapter.remove((Experiment) experimentList.getItemAtPosition(info.position));
            return true;
        }
        return super.onContextItemSelected(item);
    }
}
