package kpc.api.computer;

import kawa.standard.Scheme;
import kpc.api.ComputerPosition;
import kpc.api.State;
import kpc.api.fs.FileSystem;

public interface Computer{
    public State state();
    public int id();
    public void turnOn();
    public void shutdown();
    public void reboot();
    public void queueEvent(String event, Object... params);
    public Terminal terminal();
    public OperatingSystem os();
    public Scheme scheme();
    public FileSystem fs();
    public ComputerPosition pos();
}