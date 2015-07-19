package kpc.api.fs;

import kpc.api.fs.io.InputStream;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

public interface Mount{
    public InputStream openInputStream(String path)
    throws IOException;

    public Path resolve(String path);
    public void list(String path, List<String> files);
    public boolean exists(String path);
    public boolean isDirectory(String path);
}