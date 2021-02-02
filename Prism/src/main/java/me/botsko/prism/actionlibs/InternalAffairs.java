package me.botsko.prism.actionlibs;

import me.botsko.prism.Prism;
import me.botsko.prism.PrismLogHandler;
import org.bukkit.Bukkit;
import org.bukkit.scheduler.BukkitScheduler;

public class InternalAffairs implements Runnable {

    private final Prism plugin;

    public InternalAffairs(Prism plugin) {
        PrismLogHandler.debug("[InternalAffairs] Keeping watch over the watchers.");
        this.plugin = plugin;
    }

    @Override
    public void run() {

        if (plugin.recordingTask != null) {

            final int taskId = plugin.recordingTask.getTaskId();

            final BukkitScheduler scheduler = Bukkit.getScheduler();

            // is recording task running?
            if (scheduler.isCurrentlyRunning(taskId) || scheduler.isQueued(taskId)) {
                PrismLogHandler.debug("[InternalAffairs] Recorder is currently active. All is good.");
                return;
            }
        }

        PrismLogHandler.log("[InternalAffairs] Recorder is NOT active... checking database");

        StringBuilder result = new StringBuilder();
        if (Prism.getPrismDataSource().reportDataSource(result)) {
             PrismLogHandler.log("[Internal Affairs]" + result.toString());
             plugin.actionRecorderTask();
        }
    }
}