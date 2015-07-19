package kpc.common.tile;

import kpc.common.computer.Bus;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;

public final class TileEntityBus
extends TileEntity {
    public Bus bus;

    public TileEntityBus(int size){
        this.bus = new Bus(size);
    }

    public TileEntityBus(){
        this.bus = new Bus(8);
    }

    @Override
    public void readFromNBT(NBTTagCompound comp){
        super.readFromNBT(comp);

        NBTTagCompound busComp = comp.getCompoundTag("bus");
        this.bus = new Bus(comp.getInteger("bus_size"));
        this.bus.readFromNBT(busComp);
    }

    @Override
    public void writeToNBT(NBTTagCompound comp){
        super.writeToNBT(comp);

        NBTTagCompound busComp = new NBTTagCompound();
        busComp.setInteger("bus_size", this.bus.size);
        this.bus.writeToNBT(busComp);
        comp.setTag("bus", busComp);
    }
}