package kpc.common.blocks;

import kpc.common.tile.TileEntityDriverInventory;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.Material;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;

public final class BlockDriverInventory
extends BlockContainer{
    public BlockDriverInventory(){
        super(Material.rock);
        this.setCreativeTab(CreativeTabs.tabBlock);
        this.setBlockName("driver.inventory");
    }

    @Override
    public TileEntity createNewTileEntity(World p_149915_1_, int p_149915_2_) {
        return new TileEntityDriverInventory();
    }
}