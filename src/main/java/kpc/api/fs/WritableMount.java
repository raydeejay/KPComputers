package kpc.api.fs;

import kpc.api.fs.io.OutputStream;

import java.io.IOException;

public interface WritableMount
extends Mount{
    public OutputStream openOutputStream(String path)
    throws IOException;

    public boolean mkdir(String path);
    public boolean touch(String path);
    public boolean rm(String path);
}