package kpc.client;

import net.minecraft.util.ResourceLocation;
import truetyper.FontHelper;
import truetyper.FontLoader;
import truetyper.TrueTypeFont;

public final class Free {
    private static final ResourceLocation font = new ResourceLocation("kpc", "textures/font/free.ttf");
    private static final TrueTypeFont veraMono = FontLoader.createFont(font, 24.0F, true);

    public static float height(){
        return veraMono.getHeight();
    }

    public static float width(String str){
        return veraMono.getWidth(str);
    }

    public static void drawString(String str, int x, int y){
        FontHelper.drawString(str, x, y, veraMono, 1.0F, 1.0F);
    }

    public static void drawString(String str, int x, int y, int color){
        int r = (color >> 16 & 0xFF);
        int g = (color >> 8 & 0xFF);
        int b = (color & 0xFF);
        FontHelper.drawString(str, x, y, veraMono, 1.0F, 1.0F, new float[]{r / 255, g / 255, b / 255});
    }
}