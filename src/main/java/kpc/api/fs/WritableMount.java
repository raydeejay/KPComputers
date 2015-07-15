package kpc.api.fs;

import java.io.IOException;
import java.io.OutputStream;

public interface WritableMount
extends Mount{
    public OutputStream openOutputStream(String path)
    throws IOException;

    public boolean mkdir(String path);
    public boolean touch(String path);
}