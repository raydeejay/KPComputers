package kpc.common.computer;

import kpc.api.driver.Driver;
import kpc.api.driver.DriverTypeRegistry;
import kpc.common.utils.DriverAddress;
import kpc.common.utils.INBTTaggable;
import net.minecraft.nbt.NBTTagCompound;

import java.lang.reflect.Constructor;
import java.util.LinkedList;
import java.util.List;

public final class Bus
implements INBTTaggable{
    private final Driver[] drivers;
    public final int size;

    public Bus(int size){
        this.size = size;
        this.drivers = new Driver[size];
    }

    public Driver atAddress(int address){
        return this.drivers[address];
    }

    public void setAtAddress(int address, Driver p) {
        this.drivers[address] = p;
    }

    public List<DriverAddress> connections(){
        List<DriverAddress> drivers = new LinkedList<>();
        for(int i = 0; i < this.drivers.length; i++){
            drivers.add(new DriverAddress(i, this.drivers[i] != null ? this.drivers[i].getType() : "null"));
        }
        return drivers;
    }

    @Override
    public void readFromNBT(NBTTagCompound comp) {
        for(int address = 0; address < this.drivers.length; address++){
            comp.setString("address_" + address, (this.drivers[address] != null ? this.drivers[address].getType() : "null"));
        }
    }

    @Override
    public void writeToNBT(NBTTagCompound comp) {
        for(int address = 0; address < this.drivers.length; address++){
            String type = comp.getString("address_" + address);
            if(type.equals("null")){
                this.drivers[address] = null;
            } else{
                this.drivers[address] = this.newInstance(DriverTypeRegistry.ofType(type));
            }
        }
    }

    private Driver newInstance(Class<? extends Driver> dClass){
        try{
            Constructor<Driver> driverConstructor = (Constructor<Driver>) dClass.getDeclaredConstructor();
            driverConstructor.setAccessible(true);
            return driverConstructor.newInstance();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}