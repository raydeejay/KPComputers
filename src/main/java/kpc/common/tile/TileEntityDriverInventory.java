package kpc.common.tile;

import kpc.api.computer.Computer;
import kpc.api.driver.Driver;
import net.minecraft.tileentity.TileEntity;

public final class TileEntityDriverInventory
extends TileEntity
implements Driver{
    @Override
    public String getType() {
        return "inventory";
    }

    @Override
    public String[] getMethods() {
        return new String[]{
          "list"
        };
    }

    @Override
    public Object invoke(String name, String[] args) {
        switch(name){
            case "list":{
                return "Hello World";
            }
        }

        return null;
    }

    @Override
    public void onConnect(Computer computer) {

    }

    @Override
    public void onDisconnect(Computer computer) {

    }

    @Override
    public int getAddress() {
        return 0x0;
    }
}