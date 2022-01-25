package com.example.simpleparadox.listycity;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A container for Cities that has helper functions.
 */
public class CityList {
    private List<City> cities = new ArrayList();

    /**
     * This adds a city to the list if the city does not exist
     *
     * @param city Candidate city to add
     */
    public void add(City city) {
        if (cities.contains(city)) {
            throw new IllegalArgumentException();
        } else {
            cities.add(city);
        }
    }

    /**
     * This returns a sorted list of cities
     *
     * @return return the sorted list
     */
    public List<City> getCities() {
        List<City> list = cities;
        Collections.sort(list);
        return list;
    }

    /**
     * This method checks if the list contains the specified city
     *
     * @param city City to check existence of
     * @return Returns true if found, false otherwise.
     */
    public boolean hasCity(City city) {
        return this.cities.contains(city);
    }

    /**
     * This method deletes the specified city from the list
     * Throws an exception when city is not in the list.
     *
     * @param city City to remove
     */
    public void delete(City city) {
        if (cities.contains(city)) {
            cities.remove(city);
        } else {
            throw new IllegalArgumentException();
        }
    }

    /**
     * This method returns the number of cities contained in the list
     *
     * @return integer with count of cities.
     */
    public int countCities() {
        return this.cities.size();
    }
}
