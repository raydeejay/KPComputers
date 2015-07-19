package kpc.common.computer.fs;

import kpc.api.fs.WritableMount;
import kpc.api.fs.io.InputStream;
import kpc.api.fs.io.OutputStream;
import net.minecraftforge.common.DimensionManager;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.List;

public final class Ext9001UsrMount
implements WritableMount{
    private final Path dir;

    public Ext9001UsrMount()
    throws IOException {
        this.dir = DimensionManager.getCurrentSaveRootDirectory().toPath().resolve("ext9001");
        Files.createDirectories(this.dir);
        Files.createDirectories(this.dir.resolve("usr"));
        Files.createDirectories(this.dir.resolve("usr").resolve("bin"));
        Files.createDirectories(this.dir.resolve("usr").resolve("home"));
    }

    @Override
    public Path resolve(String path){
        Path ret = this.dir.resolve(path.startsWith("/usr/") ? path.substring(5): path);
        if(!Files.exists(ret)){
            ret = this.dir.resolve("usr/" + (path.startsWith("/usr/") ? path.substring(5) : path));
        }
        return ret;
    }

    @Override
    public OutputStream openOutputStream(String path)
    throws IOException{
        try{
            return new OutputStream(Files.newOutputStream(this.dir.resolve(path), StandardOpenOption.CREATE, StandardOpenOption.WRITE));
        } catch(Exception e){
            e.printStackTrace(System.err);
            return null;
        }
    }

    @Override
    public boolean mkdir(String path) {
        Path newDir = this.dir.resolve(path);
        try {
            Files.createDirectories(newDir);
            return true;
        } catch (IOException e) {
            return false;
        }
    }

    @Override
    public boolean touch(String path) {
        Path newFile = this.dir.resolve(path);
        try{
            Files.createFile(newFile);
            return true;
        } catch(Exception e){
            return false;
        }
    }

    @Override
    public boolean rm(String path) {
        Path file = this.dir.resolve(path);

        if(Files.isDirectory(file)){
            return false;
        }

        try{
            Files.deleteIfExists(file);
            return true;
        } catch(Exception e){
            return false;
        }
    }

    @Override
    public InputStream openInputStream(String path)
    throws IOException {
        try{
            return new InputStream(Files.newInputStream(this.dir.resolve(path)));
        } catch(Exception e){
            return null;
        }
    }

    @Override
    public void list(String path, List<String> files) {
        try{
            Path p = this.resolve(path);

            if(!Files.exists(p)){
                throw new FileNotFoundException("Couldn't resolve " + p);
            }

            try(DirectoryStream<Path> stream = Files.newDirectoryStream(p)){
                for(Path file : stream){
                    files.add(file.getFileName().toString());
                }
            }
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean exists(String path) {
        return Files.exists(this.dir.resolve(path));
    }

    @Override
    public boolean isDirectory(String path) {
        return Files.isDirectory(this.dir.resolve(path));
    }
}