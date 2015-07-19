package kpc.common.core;

import kpc.common.computer.ClientComputer;

public final class ClientComputerRegistry
extends ComputerRegistry<ClientComputer>{
    @Override
    public void update() {
        // Fallthrough
    }

    @Override
    public void register(int id, ClientComputer comp){
        super.register(id, comp);
        comp.requestState();
    }
}