package kpc.api.computer;

public interface Terminal{
    public int getHeight();
    public int getWidth();
    public int getBackgroundColor();
    public int getForegroundColor();
    public int getCursorX();
    public int getCursorY();
    public String getLine(int line);
    public void write(String line);
    public void clear();
    public void backspace();
    public void setCursorPos(int x, int y);
}