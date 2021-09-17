class LowerCaseSpeakDecorator implements Speak {

    Speak speak;

    LowerCaseSpeakDecorator(Speak delegate) {
        speak = delegate;
    }

    public String speak() {
        return speak.speak().toLowerCase();
    }

}

// Main function:
Speaker s = new Speaker();

s.speak = new LowerCaseSpeakDecorator(new Tick());

s.speak();
