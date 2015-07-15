package kpc.common.core;

import kpc.common.computer.ServerComputer;

public final class ServerComputerRegistry
extends ComputerRegistry<ServerComputer>{
    public void update(){
        for (ServerComputer serverComputer : this.all()) {
            serverComputer.broadcastState();
        }
    }

    @Override
    public void register(int id, ServerComputer computer){
        super.register(id, computer);
        computer.broadcastState();
    }

    @Override
    public void remove(int id){
        ServerComputer comp = this.get(id);
        if(comp != null){
            comp.broadcastDelete();
        }
        super.remove(id);
    }
}