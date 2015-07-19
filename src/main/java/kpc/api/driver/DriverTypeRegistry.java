package kpc.api.driver;

import java.util.HashMap;
import java.util.Map;

public final class DriverTypeRegistry{
    private static final Map<String, Class<? extends Driver>> driverTypes = new HashMap<>();

    public static Class<? extends Driver> ofType(String type){
        return driverTypes.get(type);
    }

    public static void register(String type, Class<? extends Driver> driver){
        driverTypes.put(type, driver);
    }
}