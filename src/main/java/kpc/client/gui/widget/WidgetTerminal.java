package kpc.client.gui.widget;

import kpc.api.Computer;
import kpc.client.Luxi;
import kpc.common.computer.Terminal;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Gui;

public final class WidgetTerminal
extends Gui{
    private final Computer computer;

    public WidgetTerminal(Computer computer){
        this.computer = computer;
    }

    public void draw(int x, int y){
        Terminal terminal = this.computer.terminal();
        int dY = y;
        for(int i = 0; i < terminal.height; i++){
            Luxi.drawString(terminal.getLine(i), x, dY, terminal.getForegroundColor());
            dY += 16;
        }

        if((Minecraft.getSystemTime() % 700) < 350){
            Luxi.drawString("_", x + 8 + terminal.getCursorX() * 6, y + 16 * terminal.getCursorY(), terminal.getForegroundColor());
        }
    }
}