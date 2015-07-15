package kpc.common.utils;

import net.minecraft.nbt.NBTTagCompound;

public interface INBTTaggable {
    public void readFromNBT(NBTTagCompound comp);
    public void writeToNBT(NBTTagCompound comp);
}