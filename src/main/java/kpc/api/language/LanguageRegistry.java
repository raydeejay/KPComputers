package kpc.api.language;

import java.util.HashMap;
import java.util.Map;

public final class LanguageRegistry{
    private static final Map<String, LanguageRuntime> runtimes = new HashMap<>();

    public static void register(String ext, LanguageRuntime runtime){
        if(ext != null && !ext.trim().isEmpty()){
            runtimes.put(ext, runtime);
        }
    }

    public static LanguageRuntime getLanguage(String ext){
        return runtimes.get(ext);
    }
}