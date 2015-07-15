package kpc.client.gui;

import kpc.api.Computer;
import kpc.client.gui.widget.WidgetTerminal;
import kpc.common.tile.TileEntityComputer;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.ScaledResolution;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.texture.DynamicTexture;
import net.minecraft.util.ChatAllowedCharacters;
import net.minecraft.util.ResourceLocation;
import org.lwjgl.input.Keyboard;
import org.lwjgl.opengl.GL11;

import javax.imageio.ImageIO;

public final class GuiComputer
extends GuiScreen{
    public static final int SCREEN_WIDTH = 800;
    public static final int SCREEN_HEIGHT = 600;
    public static final ResourceLocation TEXTURE;
    static{
        try{
            TEXTURE = Minecraft.getMinecraft().renderEngine.getDynamicTextureLocation("kpc", new DynamicTexture(ImageIO.read(System.class.getResourceAsStream("/assets/kpc/textures/gui/display.png"))));
        } catch(Exception e){
            throw new RuntimeException(e);
        }
    }

    private final TileEntityComputer tile;
    private final Computer computer;
    private WidgetTerminal widgetTerminal;

    public GuiComputer(TileEntityComputer tile){
        this.tile = tile;
        this.computer = tile.createComputer();
    }

    @Override
    public void initGui(){
        super.initGui();
        this.widgetTerminal = new WidgetTerminal(this.computer);
    }

    @Override
    public void drawScreen(int mX, int mY, float f){
        this.drawDefaultBackground();
        ScaledResolution scaledResolution = new ScaledResolution(this.mc, this.mc.displayWidth, this.mc.displayHeight);
        int scaleFactor = scaledResolution.getScaleFactor();

        int dX = (this.mc.displayWidth / 2) - (SCREEN_WIDTH / 2);
        int dY = (this.mc.displayHeight / 2) - (SCREEN_HEIGHT / 2);

        GL11.glColor3f(1.0F, 1.0F, 1.0F);
        GL11.glScalef(4.0F / (float) scaleFactor, 4.0F / (float) scaleFactor, 0.0F);
        this.mc.renderEngine.bindTexture(TEXTURE);
        this.drawTexturedModalRect(dX / 4 - 8, dY / 4 - 8, 0, 0, 256, 256);
        this.drawColoredQuad(this.computer.terminal().getBackgroundColor(), dX / 4, dY / 4, this.zLevel, 200, 150);

        int x = dX / 4;
        int y = dY / 4;
        this.widgetTerminal.draw(x + 70, y + 15);
    }

    @Override
    public boolean doesGuiPauseGame() {
        return false;
    }

    @Override
    public void keyTyped(char c, int code){
        if(code == Keyboard.KEY_ESCAPE){
            this.mc.setIngameFocus();
            return;
        }

        if(code == Keyboard.KEY_RETURN){
            if(this.computer != null){
                this.computer.queueEvent("char", "__enter__", Keyboard.KEY_RETURN);
                return;
            }
        }

        if(code == Keyboard.KEY_BACK){
            if(this.computer != null){
                this.computer.queueEvent("char", "__back__", Keyboard.KEY_BACK);
                return;
            }
        }

        if(ChatAllowedCharacters.isAllowedCharacter(c)){
            if(this.computer != null){
                this.computer.queueEvent("char", Character.toString(c), code);
            }
        }
    }

    private void drawColoredQuad(int color, int alpha, double x, double y, double z, double width, double height){
        Tessellator tess = Tessellator.instance;
        tess.startDrawingQuads();

        int r = (color >> 16 & 0xFF);
        int g = (color >> 8 & 0xFF);
        int b = (color & 0xFF);

        GL11.glDisable(GL11.GL_TEXTURE_2D);
        tess.setColorRGBA(r, g, b, alpha);
        tess.addVertex(x, y + height, z);
        tess.addVertex(x + width, y + height, z);
        tess.addVertex(x + width, y, z);
        tess.addVertex(x, y, z);
        tess.draw();
        GL11.glEnable(GL11.GL_TEXTURE_2D);
    }

    private void drawColoredQuad(int color, double x, double y, double z, double width, double height){
        this.drawColoredQuad(color, 255, x, y, z, width, height);
    }
}