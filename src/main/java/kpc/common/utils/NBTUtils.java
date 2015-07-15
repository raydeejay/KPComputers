package kpc.common.utils;

import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagByte;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagDouble;
import net.minecraft.nbt.NBTTagString;

import java.util.HashMap;
import java.util.Map;

public final class NBTUtils{
    private NBTUtils(){}

    private static NBTBase toNBTTag(Object obj){
        if(obj != null){
            if(obj instanceof Boolean){
                boolean b = (Boolean) obj;
                return new NBTTagByte((byte) (b ? 1 : 0));
            } else if(obj instanceof Number){
                double d = ((Number) obj).doubleValue();
                return new NBTTagDouble(d);
            } else if(obj instanceof String){
                return new NBTTagString(obj.toString());
            } else if(obj instanceof Map){
                Map<Object, Object> m = (Map) obj;
                NBTTagCompound comp = new NBTTagCompound();
                int len = 0;
                for(Map.Entry<Object, Object> entry : m.entrySet()){
                    NBTBase key = toNBTTag(entry.getKey());
                    NBTBase value = toNBTTag(entry.getValue());
                    if(key != null && value != null){
                        comp.setTag("k" + len, key);
                        comp.setTag("v" + len, value);
                        len++;
                    }
                }

                comp.setInteger("len", m.size());
                return comp;
            }
        }

        return null;
    }

    public static NBTTagCompound encode(Object[] objs){
        if(objs != null && objs.length > 0){
            NBTTagCompound comp = new NBTTagCompound();
            comp.setInteger("len", objs.length);

            for(int i = 0; i < objs.length; i++){
                NBTBase tag = toNBTTag(objs[i]);
                if(tag != null){
                    comp.setTag("" + i, tag);
                }
            }

            return comp;
        }

        return null;
    }

    private static Object fromNBT(NBTBase tag){
        if(tag != null){
            byte type = tag.getId();
            switch(type){
                case 1:{
                    return ((NBTTagByte) tag).func_150290_f() > 0;
                }
                case 6:{
                    return ((NBTTagDouble) tag).func_150286_g();
                }
                case 8:{
                    return ((NBTTagString) tag).func_150285_a_();
                }
                case 10:{
                    NBTTagCompound comp = (NBTTagCompound) tag;
                    int len = comp.getInteger("len");
                    Map<Object, Object> map = new HashMap<>(len);
                    for(int i = 0; i < len; i++){
                        Object key = fromNBT(comp.getTag("k" + i));
                        Object value = fromNBT(comp.getTag("v" + i));

                        if(key != null && value != null){
                            map.put(key, value);
                        }
                    }
                    return map;
                }
            }

        }

        return null;
    }

    public static Object[] decode(NBTTagCompound comp){
        int len = comp.getInteger("len");
        if(len > 0){
            Object[] objs = new Object[len];
            for(int i = 0; i < len; i++){
                String key = "" + i;
                if(comp.hasKey(key)){
                    NBTBase tag = comp.getTag(key);
                    objs[i] = fromNBT(tag);
                }
            }
            return objs;
        }
        return null;
    }
}