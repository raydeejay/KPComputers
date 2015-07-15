package kpc.api.language;

import java.io.InputStream;

public interface LanguageRuntime{
    public Object eval(String line)
    throws Exception;

    public Object run(InputStream stream, Object... args)
    throws Exception;
}