package kpc.common.container;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;

public final class ContainerComputer
extends Container{
    @Override
    public boolean canInteractWith(EntityPlayer p_75145_1_) {
        return true;
    }
}