package kpc.common.computer.api;

import kpc.api.fs.FileSystem;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public final class FileSystemApi{
    private final FileSystem fs;

    public FileSystemApi(FileSystem fs){
        this.fs = fs;
    }

    public boolean exists(String path){
        return this.fs.exists(path);
    }

    public boolean isDirectory(String path){
        return this.fs.isDirectory(path);
    }

    public String list(String path){
        List<String> files = new LinkedList<>();
        try {
            this.fs.list(path, files);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return files.toString();
    }

    public void touch(String path){
        this.fs.touch(path);
    }
}