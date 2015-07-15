package kpc.api;

import kpc.common.computer.Terminal;

public interface Computer{
    public int id();
    public void turnOn();
    public void shutdown();
    public void reboot();
    public void queueEvent(String event, Object... params);
    public Terminal terminal();
}