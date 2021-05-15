package com.n30phyte.listycity;

import java.io.Serializable;

public class City implements Serializable {
    String city;
    String province;

    public City(String city, String province) {
        this.city = city;
        this.province = province;
    }

    public String getCity() {
        return city;
    }

    public String getProvince() {
        return province;
    }
}
