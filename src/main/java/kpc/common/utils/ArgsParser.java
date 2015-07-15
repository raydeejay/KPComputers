package kpc.common.utils;

import java.util.LinkedList;
import java.util.List;

public final class ArgsParser{
    private ArgsParser(){}

    public static String[] parse(String str){
        List<String> args = new LinkedList<>();

        boolean paren = false;
        String s = "";
        for(char c : str.toCharArray()){
            if(c == '\"'){
                if(paren){
                    args.add(s);
                    paren = false;
                } else{
                    s = "";
                    paren = true;
                }
            } else{
                s += c;
            }
        }

        return args.toArray(new String[args.size()]);
    }
}