package kpc.common.computer.api;

import kpc.common.computer.Terminal;

public final class TerminalApi {
    private final Terminal terminal;

    public TerminalApi(Terminal terminal){
        this.terminal = terminal;
    }

    public void setCursorPos(int x, int y){
        this.terminal.setCursorPos(x, y);
    }

    public void clear(){
        this.terminal.clear();
    }

    public int getHeight(){
        return this.terminal.getHeight();
    }

    public int getWidth(){
        return this.terminal.getWidth();
    }

    public int setForeground(String c){
        int color = Integer.parseInt(c.substring(2), 16);
        this.terminal.setForegroundColor(color);
        return color;
    }

    public void write(Object o){
        if(o != null){
            this.terminal.write(o.toString());
        }
    }

    public void clearLine(){
        this.terminal.clearLine();
    }

    public int setBackground(String c){
        int color = Integer.parseInt(c.substring(2), 16);
        this.terminal.setBackgroundColor(color);
        return color;
    }

    public void backspace(){
        this.terminal.backspace();
    }

    public int getCursorX(){
        return this.terminal.getCursorX();
    }

    public int getCursorY(){
        return this.terminal.getCursorY();
    }
}