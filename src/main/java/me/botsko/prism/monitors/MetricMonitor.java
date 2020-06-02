package me.botsko.prism.monitors;

import me.botsko.prism.Prism;
import org.bstats.bukkit.Metrics;
import org.bukkit.Bukkit;
import org.bukkit.scheduler.BukkitTask;

import java.util.HashMap;
import java.util.Map;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 2/06/2020.
 */
public class MetricMonitor {

    private static boolean enabled = false;
    private  BukkitTask task;
    private static final Map<String,Integer> actions = new HashMap<>();
    private static final Object lock = new Object();

    /**
     * Monitor Class to handles Bstats Metrics.
     */
    public MetricMonitor() {
        if (Prism.getInstance().getConfig().getBoolean("prism.allow-metrics")) {
            Prism.log("Prism bStats metrics are enabled - thank you!");
            int pluginid = 4365; // assigned by bstats.org
            Metrics metrics = new Metrics(Prism.getInstance(), pluginid);
            enabled = metrics.isEnabled();
            if (!enabled) {
                Prism.warn("bStats failed to initialise! Please check Prism/bStats configs.");
                return;
            }
            Metrics.CustomChart chart = new Metrics.DrilldownPie("logged_actions", () -> {
                Map<String,Map<String, Integer>> out = new HashMap<>();
                HashMap<String,Integer> enabled = new HashMap<>();
                HashMap<String,Integer> disabled = new HashMap<>();
                out.put("enabled",enabled);
                out.put("disabled",disabled);
                for (String action: Prism.getActionRegistry().listAll()) {
                    if (Prism.getIgnore().event(action)) {
                        enabled.put(action,actions.get(action));
                    } else {
                        disabled.put(action,actions.getOrDefault(action,0));
                    }
                }
                return out;
            });
            metrics.addCustomChart(chart);
            task = Bukkit.getScheduler()
                    .runTaskTimerAsynchronously(Prism.getInstance(), this::clearActions, 72000, 72000);
            return;
        }
        task = null;
    }

    private void clearActions() {
        synchronized (lock) {
            actions.replaceAll((s, integer) -> 0);
        }
    }

    /**
     * Record an action.
     * @param actionType the type.
     */
    public static void recordAction(String actionType) {
        if (enabled) {
            synchronized (lock) {
                Integer i = actions.get(actionType);
                actions.put(actionType, i + 1);
            }
        }
    }

    /**
     * Disable the Monitor.
     */
    public void disable() {
        if (task != null && !task.isCancelled()) {
            Bukkit.getScheduler().cancelTask(task.getTaskId());
        }
    }
}
