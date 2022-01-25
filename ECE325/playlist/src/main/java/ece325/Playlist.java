package ece325;

import java.util.HashSet;
import java.util.Set;

/**
 * Assignment 6: Test Driven Development <br /> The {@code Playlist} class
 */
// @SuppressWarnings("serial")
public class Playlist<E extends Song> extends java.util.ArrayList<E> {
    private final String title;

    public Playlist(String title) {
        this.title = title;
    }

    public String getTitle() {
        return title;
    }

    public boolean addtoPlist(E song) {
        if (song == null) {
            return false;
        }

        if (this.findSong(song) != -1) {
            return false;
        }

        return this.add(song);
    }


    public boolean removeFromPlist(E song) {
        return this.remove(song);
    }

    public E getSong(int idx) {
        return this.get(idx);
    }

    public boolean hasTitle(String playlistTitle) {
        return this.title.equals(playlistTitle);
    }

    public boolean hasArtist(String artist) {
    
        for (E song : this) {
            if (song.isArtist(artist)) {
                return true;
            }
        }
        
        return false;
    }

    public int numberOfSongs() {
        return this.size();
    }


    public int numberOfArtists() {
        var artists = new HashSet<String>();

        for (E song : this) {
            artists.add(song.getArtist().toLowerCase());
        }

        return artists.size();
    }

    public int numberOfTitles() {
        var titles = new HashSet<String>();

        for (E song : this) {
            titles.add(song.getTitle().toLowerCase());
        }

        return titles.size();
    }

    public double playTime() {
        double time = 0;
        
        for (E song : this) {
            time += song.getLength();
        }

        return time;
    }

    public int findSong(E song) {
        for (var i = 0; i < this.size(); i++) {
            if (this.getSong(i).equals(song)) {
                return i;
            }
        }
        return -1;
    }

    public void sortByArtist() {
        this.sort((E song1, E song2) -> song1.getArtist().toLowerCase()
            .compareTo(song2.getArtist().toLowerCase()));
    }

    public void sortByTitle() {
        this.sort((E song1, E song2) -> song1.getTitle().toLowerCase()
            .compareTo(song2.getTitle().toLowerCase()));
    }
}
