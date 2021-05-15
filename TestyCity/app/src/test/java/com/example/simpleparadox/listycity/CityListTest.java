package com.example.simpleparadox.listycity;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CityListTest {

    private CityList mockCityList() {
        CityList cityList = new CityList();
        cityList.add(mockCity());
        return cityList;
    }

    private City mockCity() {
        return new City("Edmonton", "Alberta");
    }

    @Test
    void testAdd() {
        CityList cityList = mockCityList();

        assertEquals(1, cityList.getCities().size());

        City city = new City("Regina", "Saskatchewan");
        cityList.add(city);

        assertEquals(2, cityList.getCities().size());
        assertTrue(cityList.getCities().contains(city));
    }

    @Test
    void testAddException() {
        CityList cityList = mockCityList();
        City city = new City("Yellowknife", "Northwest Territories");
        cityList.add(city);
        assertThrows(IllegalArgumentException.class, () -> cityList.add(city));
    }

    @Test
    void testGetCities() {
        CityList cityList = mockCityList();

        assertEquals(0, mockCity().compareTo(cityList.getCities().get(0)));

        City city = new City("Charlottetown", "Prince Edward Island");

        cityList.add(city);

        assertEquals(0, city.compareTo(cityList.getCities().get(0)));
        assertEquals(0, mockCity().compareTo(cityList.getCities().get(1)));
    }

    @Test
    void testHasCities() {
        CityList cityList = mockCityList();

        City city = new City("Vancouver", "British Columbia");

        assertFalse(cityList.hasCity(city));

        cityList.add(city);
        assertTrue(cityList.hasCity(city));
    }

    @Test
    void testDeleteCity() {
        CityList cityList = mockCityList();

        City city = new City("Vancouver", "British Columbia");
        City mockCity = mockCity();

        assertEquals(1, cityList.getCities().size());
        assertDoesNotThrow(() -> cityList.delete(mockCity));
        assertThrows(IllegalArgumentException.class, () -> cityList.delete(city));
        assertEquals(0, cityList.getCities().size());
    }

    @Test
    void testCountCity() {
        CityList cityList = mockCityList();

        City city = new City("Vancouver", "British Columbia");
        assertEquals(cityList.getCities().size(), cityList.countCities());
    }
}