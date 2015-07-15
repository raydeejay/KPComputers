package kpc.common.computer;

import kpc.common.utils.NBTUtils;
import kpc.common.KPComputers;
import kpc.common.net.KPCPacket;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.ChunkCoordinates;
import net.minecraft.world.World;

public final class ServerComputer
implements kpc.api.Computer{
    private final int id;
    private final Terminal terminal;
    private final Computer computer;
    private World world;
    private ChunkCoordinates pos;

    public ServerComputer(World world, int id){
        this.id = id;
        this.terminal = new Terminal(200, 150);
        this.computer = new Computer(this.terminal);
        this.world = world;
        this.pos = null;
    }

    public World getWorld(){
        return this.world;
    }

    public void setWorld(World world){
        this.world = world;
    }

    public ChunkCoordinates getPosition(){
        return this.pos;
    }

    public void setPosition(int x, int y, int z){
        this.pos = new ChunkCoordinates(x, y, z);
    }

    @Override
    public int id() {
        return this.id;
    }

    @Override
    public void turnOn(){
        this.computer.turnOn();
    }

    @Override
    public void shutdown(){
    }

    @Override
    public void reboot(){
    }

    @Override
    public void queueEvent(String event, Object... params) {
        this.computer.queueEvent(event, params);
    }

    @Override
    public Terminal terminal() {
        return this.terminal;
    }

    public void broadcastState(){
        KPCPacket packet = new KPCPacket();
        packet.id = KPCPacket.PACKET_COMPUPDATE;
        packet.dataInt = new int[]{this.id};
        packet.dataNBT = new NBTTagCompound();
        this.writeDescription(packet.dataNBT);
        KPComputers.channel.sendToAll(KPComputers.encode(packet));
    }

    public void sendState(EntityPlayer player){
        KPCPacket packet = new KPCPacket();
        packet.id = KPCPacket.PACKET_COMPUPDATE;
        packet.dataInt = new int[]{this.id};
        packet.dataNBT = new NBTTagCompound();
        this.writeDescription(packet.dataNBT);
        KPComputers.channel.sendTo(KPComputers.encode(packet), (EntityPlayerMP) player);
    }

    public void writeDescription(NBTTagCompound comp) {
        NBTTagCompound termComp = new NBTTagCompound();
        termComp.setInteger("width", this.terminal.width);
        termComp.setInteger("height", this.terminal.height);
        this.terminal.writeToNBT(termComp);
        comp.setTag("terminal", termComp);
    }

    public void broadcastDelete(){
        KPCPacket packet = new KPCPacket();
        packet.id = KPCPacket.PACKET_COMPDELETED;
        packet.dataInt = new int[]{this.id};
        KPComputers.channel.sendToAll(KPComputers.encode(packet));
    }

    public void handlePacket(KPCPacket packet, EntityPlayer sender){
        switch(packet.id){
            case KPCPacket.PACKET_TURNON:{
                this.turnOn();
                break;
            }
            case KPCPacket.PACKET_REBOOT:{
                this.reboot();
                break;
            }
            case KPCPacket.PACKET_SHUTDOWN:{
                this.shutdown();
                break;
            }
            case KPCPacket.PACKET_REQUESTUPDATE:{
                this.sendState(sender);
                break;
            }
            case KPCPacket.PACKET_QUEUEEVENT:{
                String event = packet.dataString[0];
                Object[] args = null;
                if(packet.dataNBT != null){
                    args = NBTUtils.decode(packet.dataNBT);
                }
                this.queueEvent(event, args);
                break;
            }
        }
    }
}