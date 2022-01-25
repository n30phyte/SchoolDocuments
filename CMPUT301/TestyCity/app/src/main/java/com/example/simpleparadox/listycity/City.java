package com.example.simpleparadox.listycity;

import androidx.annotation.NonNull;

import java.util.List;

/**
 * A class that describes a City, containing the City name and the City's province.
 */
public class City implements Comparable<City> {
    private final String city;
    private final String province;

    /**
     * Constructor for the city object
     *
     * @param city Name of the city
     * @param province Name of the province that contains the city
     */
    City(@NonNull String city, @NonNull String province) {
        this.city = city;
        this.province = province;
    }

    /**
     * Getter for city name
     *
     * @return The name of the city
     */
    String getCityName() {
        return this.city;
    }

    /**
     * Getter for Province name
     *
     * @return The name of the province
     */
    String getProvinceName() {
        return this.province;
    }

    /**
     * Compares two city names, used for {@link java.util.Collections#sort(List)}}
     * Implemented with the {@link String#compareTo(String)} function}
     * @param city Other city to compare to
     * @return -1
     */
    @Override
    public int compareTo(City city) {
        return this.city.compareTo(city.getCityName());
    }

    /**
     * equals override, to allow use of {@link java.util.List#contains(Object)} method
     *
     * @return True if two objects are equal, false otherwise
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;

        City city = (City) obj;

        if (!this.city.equals(city.getCityName())) return false;
        return province.equals(city.province);
    }

    /**
     * hashCode override, to allow use of {@link java.util.List#contains(Object)} method
     *
     * @return A unique hashcode based on the city name and province name
     */
    @Override
    public int hashCode() {
        int result = city.hashCode();
        result = 7 * result + province.hashCode();
        return result;
    }
}
