package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.events.PrismCustomPlayerActionEvent;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import java.util.List;

public class PrismCustomEvents implements Listener {

    /**
     *
     */
    private final Prism plugin;

    /**
     * @param plugin
     */
    public PrismCustomEvents(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onCustomPlayerAction(final PrismCustomPlayerActionEvent event) {
        final List<String> allowedPlugins = plugin.getConfig().getStringList("prism.tracking.api.allowed-plugins");
        if (allowedPlugins.contains(event.getPluginName())) {
            RecordingQueue.addToQueue(
                    ActionFactory.createPlayer(event.getActionTypeName(), event.getPlayer(), event.getMessage()));
        }
    }
}