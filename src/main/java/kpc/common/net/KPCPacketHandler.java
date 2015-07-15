package kpc.common.net;

import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.network.FMLNetworkEvent;
import kpc.common.KPComputers;
import net.minecraft.network.NetHandlerPlayServer;

public final class KPCPacketHandler{
    @SubscribeEvent
    public void onClientPacket(FMLNetworkEvent.ClientCustomPacketEvent e){
        try{
            KPCPacket packet = new KPCPacket();
            packet.fromBytes(e.packet.payload());
            KPComputers.proxy.handlePacket(packet, null);
        } catch(Exception ex){
            ex.printStackTrace(System.err);
        }
    }

    @SubscribeEvent
    public void onServerPacket(FMLNetworkEvent.ServerCustomPacketEvent e){
        try{
            KPCPacket packet = new KPCPacket();
            packet.fromBytes(e.packet.payload());
            KPComputers.proxy.handlePacket(packet, ((NetHandlerPlayServer) e.handler).playerEntity);
        } catch(Exception ex){
            ex.printStackTrace(System.err);
        }
    }
}