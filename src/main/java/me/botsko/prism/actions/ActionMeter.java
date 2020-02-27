package me.botsko.prism.actions;

import au.com.addstar.dripreporter.DripMeter;
import com.codahale.metrics.Meter;
import com.codahale.metrics.MetricRegistry;
import me.botsko.prism.Prism;

import java.util.HashMap;
import java.util.Map;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 25/02/2020.
 */
public class ActionMeter {
    private static Map<String, DripMeter> meter = new HashMap<>();
    private static boolean monitoring = false;

    static {
        meter.put(GenericAction.class.getName(), new DripMeter());
        meter.put(EntityAction.class.getName(), new DripMeter());
        meter.put(BlockAction.class.getName(), new DripMeter());
        meter.put(BlockChangeAction.class.getName(), new DripMeter());
        meter.put(ItemStackAction.class.getName(), new DripMeter());
        meter.put(BlockShiftAction.class.getName(), new DripMeter());
        meter.put(EntityTravelAction.class.getName(), new DripMeter());
        meter.put(GrowAction.class.getName(), new DripMeter());
        meter.put(HangingItemAction.class.getName(), new DripMeter());
        meter.put(PlayerAction.class.getName(), new DripMeter());
        meter.put(PlayerDeathAction.class.getName(), new DripMeter());
        meter.put(PrismProcessAction.class.getName(), new DripMeter());
        meter.put(PrismRollbackAction.class.getName(), new DripMeter());
        meter.put(SignAction.class.getName(), new DripMeter());
        meter.put(VehicleAction.class.getName(), new DripMeter());
    }

    /**
     * Setup the meter to record.
     * @param registry metric registry
     */
    public static void setupActionMeter(MetricRegistry registry) {
        for (Map.Entry<String, DripMeter> e : meter.entrySet()) {
            registry.register(e.getKey(), e.getValue());
        }
        monitoring = Prism.getInstance().monitoring;
    }

    @SuppressWarnings("rawtypes")
    static void mark(Class clazz) {
        if (monitoring) {
            Meter m = meter.get(clazz.getName());
            if (m == null) {
                m = meter.get(GenericAction.class.getName());
            }
            m.mark();
        }
    }
}
