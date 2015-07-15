package kpc.api.fs;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.List;

public interface FileSystem{
    public OutputStream openOutputStream(String path)
    throws IOException;

    public InputStream openInputStream(String path)
    throws IOException;

    public void list(String path, List<String> list)
    throws IOException;

    public Path resolve(String path);
    public boolean mkdir(String path);
    public boolean touch(String path);
    public boolean exists(String path);
    public boolean isDirectory(String path);
}