package com.n30phyte.listycity;

import androidx.annotation.ColorRes;
import androidx.appcompat.app.AppCompatActivity;

import android.graphics.Color;
import android.os.Bundle;
import android.util.Log;
import android.util.SparseBooleanArray;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import java.util.ArrayList;

import static android.widget.AdapterView.INVALID_POSITION;
import static android.widget.AdapterView.INVALID_ROW_ID;

public class MainActivity extends AppCompatActivity {

    ListView cityList;
    ArrayAdapter<String> cityAdapter;
    ArrayList<String> dataList = new ArrayList<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        cityList = findViewById(R.id.cityList);

        cityAdapter = new ArrayAdapter<>(this, R.layout.content, dataList);

        cityList.setAdapter(cityAdapter);

        TextView inputBox = findViewById(R.id.cityNameInput);
        Button addButton = findViewById(R.id.addButton);
        Button deleteButton = findViewById(R.id.deleteButton);

        inputBox.setOnEditorActionListener((view, id, event) -> {
            if (id == EditorInfo.IME_ACTION_DONE) {
                addButton.performClick();
                return true;
            }
            return false;
        });

        addButton.setOnClickListener((view) -> {
            String cityName = inputBox.getText().toString();
            if (!dataList.contains(cityName)) {
                dataList.add(cityName);
                inputBox.setText("");
            } else {
                Toast.makeText(getApplicationContext(), "City already exists", Toast.LENGTH_SHORT).show();
            }
            cityAdapter.notifyDataSetChanged();
        });

        deleteButton.setOnClickListener((view) -> {
            SparseBooleanArray targetItems = cityList.getCheckedItemPositions();
            for (int i = targetItems.size() - 1; i >= 0; i--) {
                // Go backwards to remove items without list changing positions.
                if (targetItems.valueAt(i)) {
                    String toDelete = cityAdapter.getItem(targetItems.keyAt(i));
                    cityAdapter.remove(toDelete);
                }
            }
            cityList.clearChoices();
            cityAdapter.notifyDataSetChanged();
        });
    }
}