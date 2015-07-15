package kpc.common;

import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.TickEvent;

public final class KPCTickHandler{
    private int tickCount = 0;

    @SubscribeEvent
    public void onTick(TickEvent.ServerTickEvent e){
        if(e.phase.equals(TickEvent.Phase.START)){
            this.tickCount++;
            if(this.tickCount == 20){
                KPComputers.serverComputerRegistry.update();
                this.tickCount = 0;
            }
        }
    }
}