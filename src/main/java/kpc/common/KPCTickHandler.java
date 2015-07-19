package kpc.common;

import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.TickEvent;
import cpw.mods.fml.common.network.FMLNetworkEvent;

public final class KPCTickHandler{
    private static KPCTickHandler instance;

    public static KPCTickHandler instance(){
        return instance == null ? instance = new KPCTickHandler() : instance;
    }

    @SubscribeEvent
    public void onConnectionOpened(FMLNetworkEvent.ClientConnectedToServerEvent e){
        KPComputers.clientComputerRegistry.reset();
    }

    @SubscribeEvent
    public void onConnectionClosed(FMLNetworkEvent.ClientDisconnectionFromServerEvent e){
        KPComputers.clientComputerRegistry.reset();
    }

    @SubscribeEvent
    public void onTick(TickEvent.ServerTickEvent e){
        if(e.phase.equals(TickEvent.Phase.START)){
            KPComputers.serverComputerRegistry.update();
        }
    }
}