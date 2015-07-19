package kpc.common.kawa;

public final class strings{
    public String sstring(String str, int x){
        if(str.isEmpty()){
            return str;
        }

        return str.substring(Math.max(0, Math.min(x, str.length())));
    }

    public String sstring(String str, int x, int y){
        if(str.isEmpty()){
            return str;
        }

        return str.substring(Math.max(0, x), Math.min(y, str.length()));
    }

    public String combine(String... args){
        String ret = args[0];
        for(int i = 1; i < args.length; i++){
            ret += args[i];
        }
        return ret;
    }
}