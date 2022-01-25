package com.n30phyte.listycity;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.DialogFragment;

public class AddCityFragment extends DialogFragment {

    private static final String ARG_NAME = "CITY";

    private OnFragmentInteractionListener listener;

    public interface OnFragmentInteractionListener {
        void onOkPressed(String city, String province);
    }

    public static AddCityFragment newInstance(City editCity) {
        AddCityFragment fragment = new AddCityFragment();

        Bundle args = new Bundle();
        args.putSerializable(ARG_NAME, editCity);
        fragment.setArguments(args);

        return fragment;
    }

    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);

        if (context instanceof OnFragmentInteractionListener) {
            listener = (OnFragmentInteractionListener) context;
        } else {
            throw new RuntimeException(context.toString() + " must implement OnFragmentInteractionListener");
        }
    }

    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
        View view = LayoutInflater.from(getActivity()).inflate(R.layout.add_city_fragment, null);
        EditText cityName = view.findViewById(R.id.input_city_name);
        EditText provinceName = view.findViewById(R.id.input_province_name);
        City newCity;

        if (getArguments() == null) {
            newCity = null;
        } else {
            newCity = (City) getArguments().getSerializable(ARG_NAME);
            cityName.setText(newCity.city);
            provinceName.setText(newCity.province);

        }

        AlertDialog.Builder builder = new AlertDialog.Builder(getContext());
        return builder.setView(view).setTitle("Add/Edit city").setNegativeButton("Cancel", null).setPositiveButton("OK", (dialog, which) -> {
            String city = cityName.getText().toString();
            String province = provinceName.getText().toString();

            if (newCity != null) {
                newCity.city = city;
                newCity.province = province;
            } else {
                listener.onOkPressed(city, province);
            }
        }).create();
    }
}
