package kpc.common.computer;

import kpc.common.utils.NBTUtils;
import kpc.common.KPComputers;
import kpc.common.net.KPCPacket;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;

public final class ClientComputer
implements kpc.api.Computer{
    private final int id;
    private Terminal terminal;

    public ClientComputer(int id){
        this.id = id;
    }

    public void requestState(){
        System.out.println("Requesting State");
        KPCPacket packet = new KPCPacket();
        packet.id = KPCPacket.PACKET_REQUESTUPDATE;
        packet.dataInt = new int[]{this.id};
        KPComputers.channel.sendToServer(KPComputers.encode(packet));
    }

    @Override
    public int id() {
        return this.id;
    }

    @Override
    public void turnOn() {
        KPCPacket packet = new KPCPacket();
        packet.id = KPCPacket.PACKET_TURNON;
        packet.dataInt = new int[]{this.id};
        KPComputers.channel.sendToServer(KPComputers.encode(packet));
    }

    @Override
    public void shutdown() {
        KPCPacket packet = new KPCPacket();
        packet.id = KPCPacket.PACKET_SHUTDOWN;
        KPComputers.channel.sendToServer(KPComputers.encode(packet));
    }

    @Override
    public void reboot() {
        KPCPacket packet = new KPCPacket();
        packet.id = KPCPacket.PACKET_REBOOT;
        packet.dataInt = new int[]{this.id};
        KPComputers.channel.sendToServer(KPComputers.encode(packet));
    }

    @Override
    public void queueEvent(String event, Object... params) {
        KPCPacket packet = new KPCPacket();
        packet.id = KPCPacket.PACKET_QUEUEEVENT;
        packet.dataInt = new int[]{this.id};
        packet.dataString = new String[]{event};
        if(params != null && params.length > 0){
            packet.dataNBT = NBTUtils.encode(params);
        }
        KPComputers.channel.sendToServer(KPComputers.encode(packet));
    }

    public void readDescription(NBTTagCompound comp){
        try{
            if(comp != null && comp.hasKey("terminal")){
                NBTTagCompound termComp = comp.getCompoundTag("terminal");
                this.terminal = new Terminal(termComp.getInteger("width"), termComp.getInteger("height"));
                this.terminal.readFromNBT(termComp);
            } else{
                this.terminal = new Terminal(200, 150);
            }
        } catch(Exception e){
            e.printStackTrace(System.err);
        }
    }

    public void handlePacket(KPCPacket packet, EntityPlayer player){
        switch(packet.id){
            case KPCPacket.PACKET_COMPUPDATE:{
                this.readDescription(packet.dataNBT);
            }
        }
    }

    @Override
    public Terminal terminal() {
        return this.terminal;
    }
}