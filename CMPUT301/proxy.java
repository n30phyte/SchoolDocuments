class ProxyFile implements OurFile {
    String name;
    OurFile file;

    ProxyFile(String name) {
        this.name = name;
    }

    ProxyFile(OurFile file) {
        this.name = file.getName();
        this.file = file;
    }

    String getName() {
        return this.name;
    }

    byte[] fileContents() {
        if (file == null) {
            this.file = new ConcreteFile(name);
        }
        
        return this.file.fileContents();
    }
}
