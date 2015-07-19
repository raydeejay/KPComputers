package kpc.common.computer.fs;

import kpc.api.fs.FileSystem;
import kpc.api.fs.Mount;
import kpc.api.fs.MountRegistry;
import kpc.api.fs.WritableMount;
import kpc.api.fs.io.InputStream;
import kpc.api.fs.io.OutputStream;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public final class Ext9001FileSystem
implements FileSystem {
    @Override
    public OutputStream openOutputStream(String path)
    throws IOException {
        try{
            path = this.sanitize(path, false);
            Mount m = this.getMount(path);
            return m instanceof WritableMount ? ((WritableMount) m).openOutputStream(path) : null;
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public InputStream openInputStream(String path)
    throws IOException {
        try{
            path = this.sanitize(path, false);
            Mount m = this.getMount(path);
            return m != null ? m.openInputStream(path) : null;
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean exists(String path) {
        try{
            path = this.sanitize(path, false);
            Mount m = this.getMount(path);
            return m != null && m.exists(path);
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean isDirectory(String path) {
        try{
            path = this.sanitize(path, false);
            Mount m = this.getMount(path);
            return m != null && m.isDirectory(path);
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean mv(String path, String to) {
        try{
            path = this.sanitize(path, false);
            to = this.sanitize(to, false);
            Mount pathMount = this.getMount(path);
            Mount toMount = this.getMount(to);

            if(!(toMount instanceof WritableMount)){
                return false;
            }

            try(java.io.InputStream in = pathMount.openInputStream(path).toInputStream();
                java.io.OutputStream out = ((WritableMount) toMount).openOutputStream(to).toOutputStream()){

                byte[] buffer = new byte[8192];
                int len;
                while((len = in.read(buffer, 0, 8192)) != -1){
                    out.write(buffer, 0, len);
                }

                return this.rm(path);
            }
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean cp(String path, String to) {
        try{
            path = this.sanitize(path, false);
            to = this.sanitize(to, false);
            Mount pathMount = this.getMount(path);
            Mount toMount = this.getMount(to);

            if(!(toMount instanceof WritableMount)){
                return false;
            }

            try(java.io.InputStream in = pathMount.openInputStream(path).toInputStream();
                java.io.OutputStream out = ((WritableMount) toMount).openOutputStream(to).toOutputStream()){

                byte[] buffer = new byte[8192];
                int len;
                while((len = in.read(buffer, 0, 8192)) != -1){
                    out.write(buffer, 0, len);
                }

                return true;
            }
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean rm(String path) {
        try{
            path = this.sanitize(path, false);
            Mount m = this.getMount(path);
            return m instanceof WritableMount && ((WritableMount) m).rm(path);
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public void list(String path, final List<String> list)
    throws IOException{
        try{
            path = this.sanitize(path, false);
            Mount m = this.getMount(path);
            m.list(path, list);
            for (Map.Entry<String, Mount> entry : MountRegistry.mounts()) {
                if (this.getDirectory(entry.getKey())
                        .equals(path)) {
                    list.add(this.getName(entry.getKey()));
                }
            }
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    private String getName(String path){
        path = this.sanitize(path, true);
        if(path.isEmpty()){
            return "/";
        }
        int lastSlash = path.lastIndexOf('/');
        if(lastSlash >= 0){
            return path.substring(lastSlash + 1);
        }
        return path;
    }

    private String getDirectory(String path){
        path = this.sanitize(path, true);
        if(path.isEmpty()){
            return "..";
        }
        int lastSlash = path.lastIndexOf('/');
        if(lastSlash >= 0){
            return path.substring(0, lastSlash);
        }
        return "";
    }

    @Override
    public Path resolve(String path) {
        path = this.sanitize(path, false);
        Mount m = this.getMount(path);
        return m.resolve(path);
    }

    @Override
    public boolean mkdir(String path) {
        try{
            path = this.sanitize(path, false);
            Mount m = this.getMount(path);
            return m instanceof WritableMount && ((WritableMount) m).mkdir(path);
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean touch(String path) {
        try {
            path = this.sanitize(path, false);
            Mount m = this.getMount(path);
            return m instanceof WritableMount && ((WritableMount) m).touch(path);
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    private String sanitize(String path, boolean wildcards){
        path = path.replace("\\", "/");
        char[] specChars = { '"', ':', '<', '>', '?', '|'};
        StringBuilder clean = new StringBuilder();
        for(int i = 0; i < path.length(); i++){
            char c = path.charAt(i);
            if((c >= ' ') && Arrays.binarySearch(specChars, c) < 0 && (wildcards || (c != '*'))){
                clean.append(c);
            }
        }
        path = clean.toString();

        String[] parts = path.split("/");
        Stack<String> output = new Stack<>();
        for (String part : parts) {
            if ((part.length() != 0) && (!part.equals("."))) {
                if (part.equals("..")) {
                    if (!output.empty()) {
                        String top = output.peek();
                        if (!top.equals("..")) {
                            output.pop();
                        } else {
                            output.push("..");
                        }
                    } else {
                        output.push("..");
                    }
                } else if (part.length() >= 255) {
                    output.push(part.substring(0, 255));
                } else {
                    output.push(part);
                }
            }
        }

        StringBuilder res = new StringBuilder("");
        Iterator<String> it = output.iterator();
        while(it.hasNext()){
            String part = it.next();
            res.append(part);
            if(it.hasNext()){
                res.append("/");
            }
        }
        return res.toString();
    }

    private Mount getMount(String path){
        Iterator<Map.Entry<String, Mount>> it = MountRegistry.mounts().iterator();
        int len = 0;
        Mount match = null;
        while(it.hasNext()){
            Map.Entry<String, Mount> next = it.next();
            if(contains(next.getKey(), path)){
                int l = toLocal(path, next.getKey()).length();
                if((match == null) || (l < len)){
                    match = next.getValue();
                    len = l;
                }
            }
        }

        if(match == null){
            throw new IllegalArgumentException("Invalid path");
        }

        return match;
    }

    private String toLocal(String path, String loc){
        path = this.sanitize(path, false);
        loc = this.sanitize(loc, false);

        String local = path.substring(loc.length());
        if(local.startsWith("/")){
            return local.substring(1);
        }
        return local;
    }

    private boolean contains(String a, String b) {
        a = this.sanitize(a, false);
        b = this.sanitize(b, false);
        return !(b.equals("..") || b.startsWith("../")) && (b.equals(a) || a.isEmpty() || b.startsWith(a + "/"));
    }
}