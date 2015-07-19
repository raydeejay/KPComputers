package kpc.common.computer;

import kawa.standard.Scheme;
import kpc.api.State;
import kpc.api.computer.OperatingSystem;
import kpc.api.fs.FileSystem;
import kpc.common.KPComputers;
import kpc.common.net.KPCPacket;
import kpc.api.ComputerPosition;
import kpc.common.utils.NBTUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;

public final class ClientComputer
implements kpc.api.computer.Computer {
    private final int id;
    private final ComputerPosition pos;
    private Terminal terminal = new Terminal();

    public ClientComputer(ComputerPosition pos, int id){
        this.pos = pos;
        this.id = id;
    }

    public void requestState(){
        KPCPacket packet = new KPCPacket();
        packet.id = KPCPacket.PACKET_REQUESTUPDATE;
        packet.dataInt = new int[]{this.id};
        KPComputers.channel.sendToServer(KPComputers.encode(packet));
    }

    @Override
    public State state() {
        throw new UnsupportedOperationException("Client state not retrievable");
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
                this.terminal = new Terminal();
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

    @Override
    public OperatingSystem os() {
        throw new UnsupportedOperationException("Client OS not retrievable");
    }

    @Override
    public Scheme scheme() {
        throw new UnsupportedOperationException("Client Scheme not retrievable");
    }

    @Override
    public FileSystem fs() {
        throw new UnsupportedOperationException("Client FileSystem not retrievable");
    }

    @Override
    public ComputerPosition pos() {
        return this.pos;
    }
}