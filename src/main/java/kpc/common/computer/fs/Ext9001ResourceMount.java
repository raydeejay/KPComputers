package kpc.common.computer.fs;

import kpc.api.fs.Mount;
import kpc.api.fs.io.InputStream;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public final class Ext9001ResourceMount
implements Mount {
    private final Path mountDir;

    public Ext9001ResourceMount(String rsrc){
        try{
            this.mountDir = Paths.get(System.class.getResource("/assets/kpc/scheme/" + rsrc).toURI());
        } catch(Exception e){
            throw new RuntimeException("Cannot find resource: /assets/kpc/scheme/" + rsrc);
        }
    }

    @Override
    public Path resolve(String path){
        if(path.equals("/") || path.equals(this.mountDir.getFileName().toString())){
            return this.mountDir;
        }
        if(path.startsWith(this.mountDir.getFileName().toString())){
            path = path.replace(this.mountDir.getFileName().toString() + "/", "");
        }
        return this.mountDir.resolve(path);
    }

    @Override
    public void list(String path, List<String> files) {
        try{
            Path p = this.resolve(path);

            try(DirectoryStream<Path> stream = Files.newDirectoryStream(p)){
                for(Path file : stream){
                    files.add(
                                     file.getFileName()
                                         .toString()
                    );
                }
            }
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public InputStream openInputStream(String path)
    throws IOException {
        Path p = this.resolve(path);
        if(Files.exists(p)){
            return new InputStream(Files.newInputStream(p));
        } else{
            return null;
        }
    }

    @Override
    public boolean exists(String path) {
        try{
            Path p = this.resolve(path);
            return Files.exists(p);
        } catch(Exception e){
            e.printStackTrace(System.err);
            return false;
        }
    }

    @Override
    public boolean isDirectory(String path) {
        try {
            Path p = this.resolve(path);
            return Files.exists(p) && Files.isDirectory(p);
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }
}