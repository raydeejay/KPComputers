package kpc.common;

import kpc.common.computer.ClientComputer;
import kpc.common.computer.ServerComputer;
import kpc.common.net.KPCPacket;
import net.minecraft.entity.player.EntityPlayer;

public class CommonProxy{
    public void init(){}

    public void handlePacket(KPCPacket packet, EntityPlayer player){
        switch(packet.id){
            case KPCPacket.PACKET_COMPUPDATE:{
                int id = packet.dataInt[0];
                if(!KPComputers.clientComputerRegistry.contains(id)){
                    KPComputers.clientComputerRegistry.register(id, new ClientComputer(id));
                }
                KPComputers.clientComputerRegistry.get(id).handlePacket(packet, player);
                break;
            }
            case KPCPacket.PACKET_COMPDELETED:{
                int id = packet.dataInt[0];
                if(KPComputers.clientComputerRegistry.contains(id)){
                    KPComputers.clientComputerRegistry.remove(id);
                }
                break;
            }
            case KPCPacket.PACKET_QUEUEEVENT:
            case KPCPacket.PACKET_REBOOT:
            case KPCPacket.PACKET_REQUESTUPDATE:
            case KPCPacket.PACKET_SHUTDOWN:
            case KPCPacket.PACKET_TURNON:{
                int id = packet.dataInt[0];
                ServerComputer comp = KPComputers.serverComputerRegistry.get(id);
                if(comp != null){
                    comp.handlePacket(packet, player);
                }
                break;
            }
        }
    }
}