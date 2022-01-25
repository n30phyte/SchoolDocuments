class MockLocation implements Location {
    public double distance(Location l) {
        return 200.0 * 1000/3600 + 1.0;
    }
}
