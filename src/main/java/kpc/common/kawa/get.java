package kpc.common.kawa;

import gnu.mapping.Procedure0;
import kpc.api.Signal;
import kpc.api.computer.OperatingSystem;
import kpc.api.computer.Terminal;

public final class get
extends Procedure0 {
    private final OperatingSystem os;
    private final Terminal terminal;

    public get(OperatingSystem os, Terminal terminal) {
        this.os = os;
        this.terminal = terminal;
    }

    @Override
    public Object apply0()
    throws Throwable {
        Signal signal = this.os.pull();
        if(signal.name().equals("char")){
            if(signal.name().equals("__enter__")){
                return null;
            } else if(signal.name().equals("__back__")){
                this.terminal.backspace();
                this.terminal.setCursorPos(this.terminal.getCursorX() - 1, this.terminal.getCursorY());
                return -1;
            } else{
                this.terminal.write("" + signal.args()[0].toString());
                this.terminal.setCursorPos(this.terminal.getCursorX() + 1, this.terminal.getCursorY());
                return signal.args()[0].toString();
            }
        } else{
            return null;
        }
    }
}