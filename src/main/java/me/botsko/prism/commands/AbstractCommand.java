package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.Bukkit;
import org.bukkit.scheduler.BukkitScheduler;

import java.util.List;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 29/04/2020.
 */
public abstract class AbstractCommand implements SubHandler {

    final StringBuilder checkIfDefaultUsed(QueryParameters parameters) {
        final List<String> defaultsUsed = parameters.getDefaultsUsed();
        StringBuilder defaultsReminder = new StringBuilder();
        if (!defaultsUsed.isEmpty()) {
            defaultsReminder.append(" using defaults:");
            for (final String d : defaultsUsed) {
                defaultsReminder.append(" ").append(d);
            }
        }
        return defaultsReminder;
    }

    @SuppressWarnings("WeakerAccess")
    protected static boolean checkRecorderActive(Prism plugin) {
        boolean recorderActive = false;
        if (plugin.recordingTask != null) {
            final int taskId = plugin.recordingTask.getTaskId();
            final BukkitScheduler scheduler = Bukkit.getScheduler();
            if (scheduler.isCurrentlyRunning(taskId) || scheduler.isQueued(taskId)) {
                recorderActive = true;
            }
        }
        return recorderActive;
    }
}
