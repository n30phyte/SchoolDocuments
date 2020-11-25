package ece325;

import java.util.Objects;

/**
 * Assignment 6: Test Driven Development <br /> The {@code Song} class
 */
public class Song {

    private final String artist;
    private final String title;
    private final double length;

    public Song(String artist, String title, double length) {
        this.artist = artist;
        this.title = title;
        this.length = length;
    }

    public String getArtist() {
        return artist;
    }

    public String getTitle() {
        return title;
    }

    public double getLength() {
        return length;
    }

    public boolean isArtist(String testArtist) {
        return artist.toLowerCase().equals(testArtist.toLowerCase());
    }

    public boolean isTitle(String testTitle) {
        return title.toLowerCase().equals(testTitle.toLowerCase());
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        Song song = (Song) obj;
        return Double.compare(song.length, length) == 0 &&
            this.isArtist(song.artist) &&
            this.isTitle(song.title);
    }

    @Override
    public int hashCode() {
        return Objects.hash(artist.toLowerCase(), title.toLowerCase(), length);
    }
}
