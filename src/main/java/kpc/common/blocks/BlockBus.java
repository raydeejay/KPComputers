package kpc.common.blocks;

import kpc.api.driver.Driver;
import kpc.common.tile.TileEntityBus;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.Material;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;

public final class BlockBus
extends BlockContainer {
    private final int size;

    public BlockBus(int size){
        super(Material.iron);
        this.size = size;
        this.setCreativeTab(CreativeTabs.tabBlock);
        this.setBlockName("bus.x" + this.size);
    }

    @Override
    public void onNeighborChange(IBlockAccess world, int x, int y, int z, int tileX, int tileY, int tileZ) {
        TileEntity tile = world.getTileEntity(x, y, z);
        TileEntity changed = world.getTileEntity(tileX, tileY, tileZ);

        if(tile instanceof TileEntityBus && changed instanceof Driver){
            ((TileEntityBus) tile).bus.setAtAddress(((Driver) changed).getAddress(), (Driver) changed);
        }
    }

    @Override
    public TileEntity createNewTileEntity(World p_149915_1_, int p_149915_2_) {
        return new TileEntityBus(this.size);
    }
}