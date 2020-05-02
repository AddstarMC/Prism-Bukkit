package me.botsko.prism.actions;

import au.com.addstar.dripreporter.DripMeter;
import com.codahale.metrics.MetricRegistry;
import me.botsko.prism.Prism;

import java.util.HashMap;
import java.util.Map;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 25/02/2020.
 */
public class ActionMeter {
    private static final Map<String, DripMeter> meter = new HashMap<>();
    private static final boolean monitoring = Prism.getInstance().monitoring;

    static {
        if (monitoring) {
            meter.put(GenericAction.class.getSimpleName(), new DripMeter());
            meter.put(EntityAction.class.getSimpleName(), new DripMeter());
            meter.put(BlockAction.class.getSimpleName(), new DripMeter());
            meter.put(BlockChangeAction.class.getSimpleName(), new DripMeter());
            meter.put(ItemStackAction.class.getSimpleName(), new DripMeter());
            meter.put(BlockShiftAction.class.getSimpleName(), new DripMeter());
            meter.put(EntityTravelAction.class.getSimpleName(), new DripMeter());
            meter.put(GrowAction.class.getSimpleName(), new DripMeter());
            meter.put(HangingItemAction.class.getSimpleName(), new DripMeter());
            meter.put(PlayerAction.class.getSimpleName(), new DripMeter());
            meter.put(PlayerDeathAction.class.getSimpleName(), new DripMeter());
            meter.put(PrismProcessAction.class.getSimpleName(), new DripMeter());
            meter.put(PrismRollbackAction.class.getSimpleName(), new DripMeter());
            meter.put(SignAction.class.getSimpleName(), new DripMeter());
            meter.put(VehicleAction.class.getSimpleName(), new DripMeter());
        }
    }

    /**
     * Setup the meter to record.
     *
     * @param registry metric registry
     */
    public static void setupActionMeter(MetricRegistry registry) {
        for (Map.Entry<String, DripMeter> e : meter.entrySet()) {
            registry.register(e.getKey(), e.getValue());
        }
    }

    @SuppressWarnings("rawtypes")
    static void mark(Class clazz) {
        if (monitoring) {
            DripMeter m = meter.get(clazz.getSimpleName());
            if (m == null) {
                m = meter.get(GenericAction.class.getSimpleName());
            }
            m.mark();
        }
    }
}
