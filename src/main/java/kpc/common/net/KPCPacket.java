package kpc.common.net;

import cpw.mods.fml.common.network.ByteBufUtils;
import cpw.mods.fml.common.network.simpleimpl.IMessage;
import io.netty.buffer.ByteBuf;
import net.minecraft.nbt.NBTTagCompound;

import java.io.UnsupportedEncodingException;

public final class KPCPacket
implements IMessage{
    public static final byte PACKET_TURNON = 0x1;
    public static final byte PACKET_REBOOT = 0x2;
    public static final byte PACKET_SHUTDOWN = 0x3;
    public static final byte PACKET_REQUESTUPDATE = 0x4;
    public static final byte PACKET_COMPUPDATE = 0x5;
    public static final byte PACKET_COMPDELETED = 0x6;
    public static final byte PACKET_QUEUEEVENT = 0x7;

    public byte id;
    public int[] dataInt;
    public NBTTagCompound dataNBT;
    public String[] dataString;

    public KPCPacket(){
        this.id = 0;
        this.dataInt = null;
        this.dataString = null;
        this.dataNBT = null;
    }

    @Override
    public void fromBytes(ByteBuf buf) {
        this.id = buf.readByte();
        byte sizeOfStrings = buf.readByte();
        byte sizeOfInts = buf.readByte();
        byte nbt = buf.readByte();

        if(sizeOfStrings == 0){
            this.dataString = null;
        } else{
            this.dataString = new String[sizeOfStrings];
            for(int i = 0; i < sizeOfStrings; i++){
                if(buf.readBoolean()){
                    int len = buf.readInt();
                    byte[] bytes = new byte[len];
                    buf.readBytes(bytes);
                    try {
                        this.dataString[i] = new String(bytes, "UTF-8");
                    } catch (UnsupportedEncodingException e) {
                        this.dataString[i] = null;
                    }
                }
            }
        }

        if(sizeOfInts == 0){
            this.dataInt = null;
        } else{
            this.dataInt = new int[sizeOfInts];
            for(int i = 0; i < sizeOfInts; i++){
                this.dataInt[i] = buf.readInt();
            }
        }

        if(nbt != 0){
            this.dataNBT = ByteBufUtils.readTag(buf);
        } else{
            System.out.println(nbt);
        }
    }

    @Override
    public void toBytes(ByteBuf buf) {
        buf.writeByte(this.id);
        if(this.dataString != null){
            buf.writeByte(this.dataString.length);
        } else{
            buf.writeByte(0);
        }

        if(this.dataInt != null){
            buf.writeByte(this.dataInt.length);
        } else{
            buf.writeByte(0);
        }

        if(this.dataNBT != null){
            buf.writeByte(1);
        } else{
            buf.writeByte(0);
        }

        if(this.dataString != null){
            for(String str : this.dataString){
                if(str != null){
                    try{
                        byte[] bytes = str.getBytes("UTF-8");
                        buf.writeBoolean(true);
                        buf.writeInt(bytes.length);
                        buf.writeBytes(bytes);
                    } catch(Exception e){
                        buf.writeBoolean(false);
                    }
                } else{
                    buf.writeBoolean(false);
                }
            }
        }

        if(this.dataInt != null){
            for(int i : this.dataInt){
                buf.writeInt(i);
            }
        }

        if(this.dataNBT != null){
            ByteBufUtils.writeTag(buf, this.dataNBT);
        }
    }
}