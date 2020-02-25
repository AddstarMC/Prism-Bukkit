package me.botsko.prism.actions;

import au.com.addstar.dripreporter.DripMeter;
import au.com.addstar.dripreporter.DripMetricSet;
import com.codahale.metrics.Meter;
import me.botsko.prism.Prism;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 25/02/2020.
 */
public class ActionMeter {
    private static DripMetricSet meters;
    public static boolean monitoring = false;

    public ActionMeter() {
        monitoring = Prism.getInstance().monitoring;
        if (monitoring) {
            meters = new DripMetricSet(Prism.class);
            meters.addMetric(new DripMeter(), GenericAction.class.getName());
            Prism.monitor.addDripMetricSet(Prism.class, meters);
        }
    }

    @SuppressWarnings("rawtypes")
    protected static void mark(Class clazz) {
        if (monitoring) {
            Meter meter = (Meter) meters.getMetrics().get(clazz.getName());
            if (meter == null) {
                meter = (Meter) meters.getMetrics().get(GenericAction.class.getName());
            }
            meter.mark();
        }
    }
}
