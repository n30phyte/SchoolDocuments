// CCID: mkwok1

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
/* DecryptInputStream can decrypt an encrypted inputstream */

class DecryptInputStream extends BufferedInputStream {
    public DecryptInputStream(InputStream in, byte[] header) {
        super(in);
        // ...
    }
    // ...
}

class HttpInputStream extends InputStream {
    public HttpInputStream(String httpURL) {
        // ...
    }

    public int read() throws IOException {
        // ...
    }
    // ...
}

class Preference {
    Preference(String strUserPrefs) {
        // ...
    }
    // ...
}

abstract class PreferenceReader { // Refactor me

    abstract void loadPrefBuffer(InputStream input) throws IOException;

    abstract void readPrefBuffer() throws IOException;

    abstract Preference getPreferences() throws IOException;

    /**
     * Reads user preferences from a specified input stream.
     */
    Preference readPref(InputStream input) throws IOException {
        this.loadPrefBuffer(input);
        this.readPrefBuffer();
        return getPreferences();
    }
}

class EncryptedFilePreferenceReader extends PreferenceReader {
    InputStream in;
    ByteArrayOutputStream buf;

    @Override
    void loadPrefBuffer(InputStream input) throws IOException {
        byte[] buffer = {0, 0, 0, 0};
        // We need the first 4 bytes for the encryption header
        // We read them and then push them back onto the stream
        // allow us to return to this part in the stream
        input.mark(0);
        // read 4 bytes to check for encrypted
        input.read(buffer, 0, 4);
        input.reset(); // unread those 4 bytes, push them back onto the stream
        in = new DecryptInputStream(input, buffer);
    }

    @Override
    void readPrefBuffer() throws IOException {
        BufferedInputStream bin = new BufferedInputStream(in);
        buf = new ByteArrayOutputStream();
        int result = bin.read();
        while (result != -1) {
            result = bin.read();
            buf.write((byte) result);
        }
    }

    @Override
    Preference getPreferences() throws IOException {
        return new Preference(buf.toString("UTF-8"));
    }
}

class UnencryptedHttpUrlPreferenceReader extends PreferenceReader {
    InputStream in;
    ByteArrayOutputStream buf;

    @Override
    void loadPrefBuffer(InputStream input) throws IOException {
        in = input;
    }

    @Override
    void readPrefBuffer() throws IOException {
        BufferedInputStream bin = new BufferedInputStream(in);
        buf = new ByteArrayOutputStream();
        int result = bin.read();
        while (result != -1) {
            result = bin.read();
            buf.write((byte) result);
        }
    }

    @Override
    Preference getPreferences() throws IOException {
        return new Preference(buf.toString("UTF-8"));
    }
}