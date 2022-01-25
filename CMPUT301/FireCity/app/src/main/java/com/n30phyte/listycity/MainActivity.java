package com.n30phyte.listycity;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;
import android.util.Log;
import android.util.SparseBooleanArray;
import android.view.ContextMenu;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import com.google.android.material.floatingactionbutton.FloatingActionButton;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.FirebaseFirestoreException;
import com.google.firebase.firestore.QueryDocumentSnapshot;
import com.google.firebase.firestore.QuerySnapshot;
import com.google.firebase.firestore.WriteBatch;

import java.util.ArrayList;
import java.util.HashMap;

public class MainActivity extends AppCompatActivity implements AddCityFragment.OnFragmentInteractionListener, View.OnCreateContextMenuListener {

    ListView cityList;
    ArrayAdapter<City> cityAdapter;
    ArrayList<City> dataList;

    CollectionReference collectionReference;
    FirebaseFirestore db;

    private static final int MENU_DELETE_ENTRY = 0;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        setupFirestore();

        cityList = findViewById(R.id.cityList);

        dataList = new ArrayList<>();
        cityAdapter = new CityAdapter(this, dataList);

        cityList.setAdapter(cityAdapter);

        registerForContextMenu(cityList);

        FloatingActionButton addCityButton = findViewById(R.id.add_city_button);

        addCityButton.setOnClickListener(
                (view) -> new AddCityFragment().show(getSupportFragmentManager(), "ADD_CITY"));
    }

    @Override
    public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
        if (v.getId() == R.id.cityList) {
            super.onCreateContextMenu(menu, v, menuInfo);
            AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) menuInfo;

            menu.add(Menu.NONE, MENU_DELETE_ENTRY, Menu.NONE, "Delete");
        }
    }

    @Override
    public boolean onContextItemSelected(MenuItem item) {
        if (item.getItemId() == MENU_DELETE_ENTRY) {
            AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) item.getMenuInfo();

            String cityName = cityAdapter.getItem(info.position).city;
            collectionReference.document(cityName).delete();
            return true;
        }
        return super.onContextItemSelected(item);
    }

    @Override
    public void onOkPressed(String city, String province) {
        HashMap<String, String> data = new HashMap<>();
        if (city.length() > 0 && province.length() > 0) {
            data.put("Province Name", province);
        }
        collectionReference.document(city)
                .set(data)
                .addOnSuccessListener(
                        aVoid -> Log.d("MainActivity", "Added new city"))
                .addOnFailureListener(
                        (Exception e) -> Log.d("MainActivity", "Could not add new city: " + e.toString()));
    }

    private void setupFirestore() {
        // Access a Cloud Firestore instance from your Activity
        db = FirebaseFirestore.getInstance();
        collectionReference = db.collection("cities");
        collectionReference.addSnapshotListener(this::onFirestoreUpdate);
    }

    private void onFirestoreUpdate(@Nullable QuerySnapshot queryDocumentSnapshots, @Nullable FirebaseFirestoreException error) {
        dataList.clear();

        for (QueryDocumentSnapshot document : queryDocumentSnapshots) {
            Log.d("MainActivity", document.getId());

            String city = document.getId();
            String province = document.getData().get("Province Name").toString();

            dataList.add(new City(city, province));
        }

        cityAdapter.notifyDataSetChanged();
    }
}