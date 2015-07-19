package kpc.common.utils;

public final class DriverAddress{
    public final int address;
    public final String driverType;

    public DriverAddress(int address, String driverType) {
        this.address = address;
        this.driverType = driverType;
    }

    @Override
    public String toString(){
        return "0x" + String.format("%02x", this.address) + ": " + this.driverType;
    }
}