package kpc.api.fs.io;

import java.io.Closeable;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public final class OutputStream
implements Closeable{
    private final java.io.OutputStream stream;

    public OutputStream(java.io.OutputStream stream){
        this.stream = stream;
    }

    public void write(String str)
    throws IOException {
        if(str == null || str.isEmpty() || str.trim().equals("\n")){
            return;
        }

        this.stream.write(str.getBytes(StandardCharsets.UTF_8));
    }

    public java.io.OutputStream toOutputStream(){
        return this.stream;
    }

    @Override
    public void close()
    throws IOException {
        this.stream.close();
    }
}