package kpc.common.kawa;

import gnu.mapping.Procedure1;
import kpc.api.computer.Terminal;

public final class print
extends Procedure1 {
    private final Terminal terminal;

    public print(Terminal terminal){
        this.terminal = terminal;
    }

    @Override
    public Object apply1(Object o)
    throws Throwable {
        if(o != null){
            this.terminal.write(o.toString());
            this.terminal.setCursorPos(this.terminal.getCursorX() + o.toString().length(), this.terminal.getCursorY());
        }
        return null;
    }
}