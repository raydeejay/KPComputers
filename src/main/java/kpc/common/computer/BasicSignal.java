package kpc.common.computer;

import kpc.api.Signal;

public final class BasicSignal
implements Signal {
    private final String name;
    private final Object[] args;

    public BasicSignal(String name, Object... args) {
        this.name = name;
        this.args = args;
    }

    @Override
    public String name() {
        return this.name;
    }

    @Override
    public Object[] args() {
        return this.args;
    }
}