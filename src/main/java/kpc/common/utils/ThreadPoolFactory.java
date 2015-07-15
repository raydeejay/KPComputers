package kpc.common.utils;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

public final class ThreadPoolFactory{
    private ThreadPoolFactory(){}

    public static ExecutorService create(final String name){
        return Executors.newFixedThreadPool(2, new ThreadFactory() {
                                                 private final ThreadGroup group = new ThreadGroup(name);
                                                 private final AtomicInteger count = new AtomicInteger(1);

                                                 @Override
                                                 public Thread newThread(Runnable runnable) {
                                                     Thread t = new Thread(group, runnable, "Worker-" + count.getAndIncrement());
                                                     t.setDaemon(true);
                                                     return t;
                                                 }
                                             }
        );
    }
}