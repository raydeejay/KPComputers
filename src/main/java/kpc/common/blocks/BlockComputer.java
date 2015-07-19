package kpc.common.blocks;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import kpc.api.computer.Computer;
import kpc.common.computer.ServerComputer;
import kpc.common.tile.TileEntityComputer;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IIconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.IIcon;
import net.minecraft.world.World;
import net.minecraftforge.common.util.ForgeDirection;

public final class BlockComputer
extends BlockContainer{
    @SideOnly(Side.CLIENT)
    private static final IIcon[] icons = new IIcon[6];

    public BlockComputer(){
        super(Material.iron);
        this.setCreativeTab(CreativeTabs.tabBlock);
        this.setHardness(5.0F);
        this.setResistance(5.0F);
        this.setBlockName("computer");
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void registerBlockIcons(IIconRegister register){
        icons[ForgeDirection.NORTH.ordinal()] = register.registerIcon("kpc:computer/front");
        icons[ForgeDirection.SOUTH.ordinal()] = register.registerIcon("kpc:computer/back");
        icons[ForgeDirection.WEST.ordinal()] = register.registerIcon("kpc:computer/right");
        icons[ForgeDirection.EAST.ordinal()] = register.registerIcon("kpc:computer/left");
        icons[ForgeDirection.DOWN.ordinal()] = register.registerIcon("kpc:computer/bottom");
        icons[ForgeDirection.UP.ordinal()] = register.registerIcon("kpc:computer/top");
    }

    @Override
    @SideOnly(Side.CLIENT)
    public IIcon getIcon(int side, int meta){
        ForgeDirection dir = ForgeDirection.getOrientation(side);
        return icons[dir.ordinal()];
    }

    @Override
    public boolean onBlockActivated(World world, int x, int y, int z, EntityPlayer player, int side, float fx, float fy, float fz) {
        ((TileEntityComputer) world.getTileEntity(x, y, z)).interact(player);
        return true;
    }

    @Override
    public void onBlockPreDestroy(World world, int x, int y, int z, int p_149725_5_) {
        super.onBlockPreDestroy(world, x, y, z, p_149725_5_);
        Computer comp = ((TileEntityComputer) world.getTileEntity(x, y, z)).createComputer();
        if(comp instanceof ServerComputer){
            ((ServerComputer) comp).broadcastDelete();
        }
    }

    @Override
    public TileEntity createNewTileEntity(World p_149915_1_, int p_149915_2_) {
        return new TileEntityComputer();
    }
}