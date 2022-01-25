interface Flyer {
    public void flap();
    public boolean isOnGround();
}

class CobraChicken implements Flyer {

private ArrayList<Feather> feathers;
private boolean onGround;

    public CobraChicken() {
        onGround = false;
        // Composition, CobraChicken handles lifetime of feathers
        feathers = new ArrayList<>();
    }

    public void flap() {}

    public boolean isOnGround() {
        return onGround;
    }
}

class CanadaGoose extends CobraChicken {
    
}

class Feather {

}

class Flock {

    private ArrayList<Flyer> flyers;

    public Flock() {
        flyers = new ArrayList<>();
    }

    public Flock(ArrayList<? implements Flyer> flyers) {
        this.flyers = flyers.
    }
}
