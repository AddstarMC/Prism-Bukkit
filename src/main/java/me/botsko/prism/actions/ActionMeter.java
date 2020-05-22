package me.botsko.prism.actions;

import au.com.addstar.dripreporter.DripMeter;
import me.botsko.prism.ApiHandler;
import me.botsko.prism.Prism;

import java.util.HashMap;
import java.util.Map;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 25/02/2020.
 */
public class ActionMeter {
    private static final Map<String, DripMeter> meter = new HashMap<>();
    private static  boolean monitoring = false;

    static {
        if (Prism.getInstance().monitoring) {
            meter.put("UnknownHandler",getMeter(Handler.class));
            meter.put(GenericAction.class.getSimpleName(), getMeter(GenericAction.class));
            meter.put(EntityAction.class.getSimpleName(), getMeter(EntityAction.class));
            meter.put(BlockAction.class.getSimpleName(),getMeter(BlockAction.class));
            meter.put(BlockChangeAction.class.getSimpleName(), getMeter(BlockChangeAction.class));
            meter.put(ItemStackAction.class.getSimpleName(), getMeter(ItemStackAction.class));
            meter.put(BlockShiftAction.class.getSimpleName(), getMeter(BlockShiftAction.class));
            meter.put(EntityTravelAction.class.getSimpleName(), getMeter(EntityTravelAction.class));
            meter.put(GrowAction.class.getSimpleName(), getMeter(GrowAction.class));
            meter.put(HangingItemAction.class.getSimpleName(), getMeter(HangingItemAction.class));
            meter.put(PlayerAction.class.getSimpleName(),getMeter(PlayerAction.class));
            meter.put(PlayerDeathAction.class.getSimpleName(), getMeter(PlayerDeathAction.class));
            meter.put(PrismProcessAction.class.getSimpleName(), getMeter(PrismProcessAction.class));
            meter.put(PrismRollbackAction.class.getSimpleName(),getMeter(PrismRollbackAction.class));
            meter.put(SignAction.class.getSimpleName(), getMeter(SignAction.class));
            meter.put(VehicleAction.class.getSimpleName(), getMeter(VehicleAction.class));
            Prism.log("Action Meter metrics enabled. " + meter.size() + " metrics registered");
            monitoring = true;
        }
    }

    @SuppressWarnings("rawtypes")
    private static DripMeter getMeter(Class clazz) {
        if (meter.containsKey(clazz.getSimpleName())) {
            return meter.get(clazz.getSimpleName());
        } else {
            return ApiHandler.monitor.addMeter(clazz);
        }
    }

    /**
     * Setup the meter to record.
     */
    public static void setupActionMeter() {
        // Initializes the static class
    }

    @SuppressWarnings("rawtypes")
    public static void mark(Class clazz) {
        if (monitoring) {
            DripMeter m = meter.get(clazz.getSimpleName());
            if (m == null) {
                m = meter.get("UnknownHandler");
            }
            if (m != null) {
                m.mark();
            }
        }
    }
}
