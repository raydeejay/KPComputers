package kpc.api.language;

import kpc.api.computer.Computer;

import java.io.Reader;

public interface LanguageRuntime{
    public Object eval(Computer caller, String line)
    throws Exception;

    public Object run(Computer caller, Reader reader, Object... args)
    throws Exception;
}