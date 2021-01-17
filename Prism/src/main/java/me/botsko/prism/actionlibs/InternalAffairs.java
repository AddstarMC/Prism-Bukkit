package me.botsko.prism.actionlibs;

import me.botsko.prism.Prism;
import org.bukkit.Bukkit;
import org.bukkit.scheduler.BukkitScheduler;

import java.sql.Connection;
import java.sql.SQLException;

public class InternalAffairs implements Runnable {

    private final Prism plugin;

    public InternalAffairs(Prism plugin) {
        Prism.debug("[InternalAffairs] Keeping watch over the watchers.");
        this.plugin = plugin;
    }

    @Override
    public void run() {

        if (plugin.recordingTask != null) {

            final int taskId = plugin.recordingTask.getTaskId();

            final BukkitScheduler scheduler = Bukkit.getScheduler();

            // is recording task running?
            if (scheduler.isCurrentlyRunning(taskId) || scheduler.isQueued(taskId)) {
                Prism.debug("[InternalAffairs] Recorder is currently active. All is good.");
                return;
            }
        }

        Prism.log("[InternalAffairs] Recorder is NOT active... checking database");

        // is db connection valid?
        try (Connection conn = Prism.getPrismDataSource().getConnection()) {
            if (conn == null) {
                Prism.log("[InternalAffairs] Pool returned NULL instead of a valid connection.");
            } else if (conn.isClosed()) {
                Prism.log("[InternalAffairs] Pool returned an already closed connection.");
            } else if (conn.isValid(5)) {
                Prism.log("[InternalAffairs] Pool returned valid connection!");
                Prism.log("[InternalAffairs] Restarting scheduled recorder tasks");
                plugin.actionRecorderTask();
            }
        } catch (final SQLException e) {
            Prism.debug("[InternalAffairs] Error: " + e.getMessage());
            e.printStackTrace();
        }
    }
}