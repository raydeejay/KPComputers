package kpc.api.language;

import java.util.HashMap;
import java.util.Map;

public final class LanguageRegistry{
    private static final Map<String, LanguageRuntime> runtimes = new HashMap<>();

    public static void register(String ext, LanguageRuntime runtime){
        if(ext != null && !ext.trim().isEmpty()){
            if(runtimes.containsKey(ext.trim())){
                return;
            }

            System.out.println("Registering Runtime For: " + ext.trim());
            runtimes.put(ext.trim(), runtime);
        }
    }

    public static LanguageRuntime getLanguage(String ext){
        if(!runtimes.containsKey(ext.trim())){
            throw new NullPointerException("Runtime for " + ext.trim() + " isn't registered");
        }

        return runtimes.get(ext.trim());
    }
}