package kpc.common.computer;

import kpc.api.Signal;
import kpc.api.language.LanguageRegistry;
import kpc.api.language.LanguageRuntime;
import kpc.common.utils.ArgsParser;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public final class OperatingSystem{
    private final Queue<Signal> signalQueue = new LinkedList<>();
    private final Lock readLock = new ReentrantLock();
    private final Lock writeLock = new ReentrantLock();
    private final Computer computer;

    protected OperatingSystem(Computer computer){
        this.computer = computer;
    }

    public String version(){
        return "v0.0";
    }

    public Object eval(String code){
        try {
            return this.computer.scheme.eval(code);
        } catch (Throwable throwable) {
            this.computer.terminal.clear();
            this.computer.terminal.setCursorPos(1, 1);
            this.computer.terminal.write("Error executing");
            this.computer.terminal.setCursorPos(1, 2);
            this.computer.terminal.write("Exception: " + throwable.getMessage());
            throwable.printStackTrace(System.err);
            return null;
        }
    }

    public Object runScript(String ext, String exec, Object... args){
        if(this.computer.fs.exists(exec.trim())){
            try(InputStream in = this.computer.fs.openInputStream(exec.trim())){
                LanguageRuntime runtime = LanguageRegistry.getLanguage(ext.trim());
                Object ret = runtime.run(in, args);
                if(ret != null){
                    return ret;
                }

                return null;
            } catch(Exception e){
                return "Exception: " + e.getMessage();
            }
        } else{
            return "Unable to locate: " + exec;
        }
    }

    public Object runScheme(String execution){
        String script = execution.contains(" ") ? execution.substring(0, execution.indexOf(' ')) : execution;
        String[] args = execution.contains(" ") ? ArgsParser.parse(execution.substring(execution.indexOf(' ') + 1)) : null;

        if(script.equals("bios.scm") || script.equals("/bios.scm")){
            return "Cannot rerun bios.scm";
        }

        if(this.computer.fs.exists(script.trim() + ".scm")){
            try(InputStream in = this.computer.fs.openInputStream(script.trim() + ".scm")){
                this.computer.scheme.define("args", args);
                Object ret = this.computer.scheme.eval(new InputStreamReader(in));
                this.computer.scheme.define("args", null);
                if(ret != null){
                    return ret;
                } else{
                    return null;
                }
            } catch(Throwable e){
                return "Exception: " + e.getMessage();
            }
        }

        if(this.computer.fs.exists("/bin/" + script.trim() + ".scm")){
            try(InputStream in = this.computer.fs.openInputStream("/bin/" + script.trim() + ".scm")){
                this.computer.scheme.define("args", args);
                Object ret = this.computer.scheme.eval(new InputStreamReader(in));
                this.computer.scheme.define("args", null);
                if(ret != null){
                    return ret;
                } else{
                    return null;
                }
            } catch(Throwable e){
                e.printStackTrace(System.err);
                return "Exception: " + e.getMessage();
            }
        }

        return "Unable to locate: " + script;
    }

    public Signal pull(){
        this.readLock.lock();
        try{
            while(this.signalQueue.isEmpty()){
                synchronized(this){
                    this.wait();
                }
            }

            return this.signalQueue.poll();
        } catch(Exception e){
            e.printStackTrace(System.err);
            return new BasicSignal("__terminate__");
        } finally{
            this.readLock.unlock();
        }
    }

    protected void signal(Signal signal){
        this.writeLock.lock();
        try{
            this.signalQueue.add(signal);
            this.notifyAll();
        } finally{
            this.writeLock.unlock();
        }
    }

    public void signal(String name, Object... args){
        this.writeLock.lock();
        try{
            this.signalQueue.add(new BasicSignal(name, args));
            this.notifyAll();
        } finally{
            this.writeLock.unlock();
        }
    }
}