package kpc.common.kawa;

import gnu.mapping.Procedure1;
import kpc.api.computer.Terminal;

public final class println
extends Procedure1 {
    private final Terminal terminal;

    public println(Terminal terminal){
        this.terminal = terminal;
    }

    @Override
    public Object apply1(Object o)
    throws Throwable {
        if(o != null){
            this.terminal.write(o.toString());
            this.terminal.setCursorPos(0, this.terminal.getCursorY() + 1);
        }
        return null;
    }
}