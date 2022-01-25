class BMPAdapter implements Image {
    BMPImage image;

    BMPAdapter(BMPImage image) {
        this.image = image;
    }

    public void setPixel(Pixel pixel) {
        int pointX = pixel.point.getX();
        int pointY = pixel.point.getY();
        Color color = pixel.color;

        image.set(pointX, pointY, color);
    }

    public Pixel getPixel(Pixel pixel) {
        int pointX = pixel.point.getX();
        int pointY = pixel.point.getY();
        pixel.color = image.get(pointX, pointY);

        return pixel;
    }
}
