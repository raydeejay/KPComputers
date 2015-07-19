package kpc.common.core;

import kpc.api.computer.Computer;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public abstract class ComputerRegistry<TComputer extends Computer>{
    private final Map<Integer, TComputer> computers = new HashMap<>();
    private int nextId = 0;

    public ComputerRegistry(){
        this.reset();
    }

    public boolean contains(int id){
        return this.computers.containsKey(id);
    }

    public abstract void update();

    public TComputer get(int id){
        if(id >= 0){
            if(this.computers.containsKey(id)){
                return this.computers.get(id);
            }
        }

        return null;
    }

    public int nextId(){
        System.out.println(this.nextId);
        return this.nextId++;
    }

    public Collection<TComputer> all(){
        return this.computers.values();
    }

    public void remove(int id){
        if(this.computers.containsKey(id)){
            this.computers.remove(id);
        }
    }

    public void register(int id, TComputer computer){
        if(this.computers.containsKey(id)){
            this.computers.remove(id);
        }

        this.computers.put(id, computer);
        this.nextId = Math.max(this.nextId, id + 1);
    }

    public void reset(){
        this.computers.clear();
        this.nextId = 0;
    }
}