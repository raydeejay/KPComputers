package kpc.api.fs.io;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStreamReader;

public final class InputStream
implements Closeable {
    private final BufferedReader reader;
    private final java.io.InputStream stream;

    public InputStream(java.io.InputStream stream){
        this.stream = stream;
        this.reader = new BufferedReader(new InputStreamReader(stream));
    }

    public Object readLine()
    throws IOException {
        String str = this.reader.readLine();
        if(str != null){
            return str;
        }

        return -1;
    }

    public java.io.InputStream toInputStream(){
        return this.stream;
    }

    @Override
    public void close()
    throws IOException {
        this.reader.close();;
    }
}