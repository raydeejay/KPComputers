package kpc.api.fs;

import com.google.common.collect.ImmutableSet;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public final class MountRegistry{
    private static final Map<String, Mount> mounts = new HashMap<>();

    public static void mount(String path, Mount mount){
        try{
            if(path.equals("..")){
                throw new IllegalAccessException("Cannot mount above the root");
            }

            if(mounts.containsKey(path)){
                return;
            }

            mounts.put(path, mount);
        } catch(IllegalAccessException e){
            throw new RuntimeException(e);
        }
    }

    public static void unmount(String path){
        try{
            if(path.equals("..")){
                throw new IllegalAccessException("Cannot mount above the root");
            }

            if(path.equals("/")){
                throw new IllegalAccessException("Cannot unmount the root mount");
            }

            if(!mounts.containsKey(path)){
                throw new NullPointerException(path + " isn't mounted");
            }

            mounts.remove(path);
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    public static Mount mount(String path){
        try{
            if(path.equals("..")){
                throw new IllegalAccessException("Cannot mount above the root");
            }

            if(!mounts.containsKey(path)){
                throw new NullPointerException(path + " isn't mounted");
            }

            return mounts.get(path);
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    public static Set<Map.Entry<String, Mount>> mounts(){
        return ImmutableSet.copyOf(mounts.entrySet());
    }
}