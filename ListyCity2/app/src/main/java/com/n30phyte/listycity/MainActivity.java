package com.n30phyte.listycity;

import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.ArrayList;

public class MainActivity extends AppCompatActivity implements AddCityFragment.OnFragmentInteractionListener {

    ListView cityList;
    ArrayAdapter<City> cityAdapter;
    ArrayList<City> dataList;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        cityList = findViewById(R.id.cityList);

        dataList = new ArrayList<>();
        cityAdapter = new CityAdapter(this, dataList);

        cityList.setAdapter(cityAdapter);

        FloatingActionButton addCityButton = findViewById(R.id.add_city_button);

        addCityButton.setOnClickListener((view) -> {
            new AddCityFragment().show(getSupportFragmentManager(), "ADD_CITY");
        });

        cityList.setOnItemClickListener((parent, view, position, id) -> {
            AddCityFragment.newInstance(dataList.get(position)).show(getSupportFragmentManager(), "EDIT_CITY");
        });
    }

    @Override
    public void onOkPressed(City newCity) {
        cityAdapter.add(newCity);
    }
}