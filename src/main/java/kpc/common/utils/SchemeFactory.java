package kpc.common.utils;

import kawa.standard.Scheme;

public final class SchemeFactory{
    private SchemeFactory(){}

    public static Scheme create(){
        Scheme scheme = new Scheme();
        scheme.getLangEnvironment().setCanDefine(true);
        scheme.getLangEnvironment().setCanRedefine(true);
        scheme.getEnvironment().setCanDefine(true);
        scheme.getEnvironment().setCanRedefine(true);
        return scheme;
    }
}