package kpc.common.computer;

import kawa.standard.Scheme;
import kpc.api.Signal;
import kpc.api.fs.FileSystem;
import kpc.common.computer.api.FileSystemApi;
import kpc.common.computer.api.TerminalApi;
import kpc.common.computer.fs.Ext9001FileSystem;
import kpc.common.kawa.include;
import kpc.common.utils.SchemeFactory;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.concurrent.ForkJoinPool;

public final class Computer{
    protected final ForkJoinPool threadPool;
    protected final OperatingSystem os;
    protected final Terminal terminal;
    protected final Scheme scheme;
    protected final FileSystem fs;

    public Computer(Terminal terminal){
        this.terminal = terminal;
        this.fs = new Ext9001FileSystem();
        this.os = new OperatingSystem(this);
        this.threadPool = new ForkJoinPool();
        this.scheme = SchemeFactory.create();

        this.scheme.define("fs", new FileSystemApi(this.fs));
        this.scheme.define("os", this.os);
        this.scheme.define("term", new TerminalApi(this.terminal));
        this.scheme.define("include", new include(this.fs));
    }

    public void turnOn(){
        this.terminal.clear();
        this.terminal.setCursorPos(1, 1);
        this.terminal.write("Loading Bios.scm");
        this.threadPool.execute(
                                  new Runnable() {
                                      @Override
                                      public void run() {
                                          init();
                                      }
                                  }
        );
    }

    public void queueEvent(final String evnt, final Object... args){
        this.threadPool.execute(
                                  new Runnable() {
                                      @Override
                                      public void run() {
                                          synchronized (this) {
                                              Signal signal = new BasicSignal(evnt, args);
                                              synchronized (os){
                                                  os.signal(signal);
                                              }
                                          }
                                      }
                                  }
        );
    }

    private void init(){
        try(InputStream stream = this.fs.openInputStream("/bios.scm")){
            if(stream != null){
                this.scheme.eval(new InputStreamReader(stream));
            } else{
                throw new NullPointerException("Couldn't resolve bios.scm");
            }
        } catch(Throwable t){
            this.terminal.clear();
            this.terminal.setCursorPos(1, 1);
            this.terminal.write("Error loading bios.scm");
            this.terminal.setCursorPos(1, 2);
            this.terminal.write("Message: " + t.getMessage());
            t.printStackTrace(System.err);
        }
    }
}