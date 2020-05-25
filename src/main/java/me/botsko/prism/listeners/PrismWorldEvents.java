package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.utils.block.Utilities;
import org.bukkit.TreeType;
import org.bukkit.block.BlockState;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.world.PortalCreateEvent;
import org.bukkit.event.world.StructureGrowEvent;
import org.bukkit.event.world.WorldLoadEvent;

public class PrismWorldEvents implements Listener {

    /**
     * StructureGrowEvent.
     * @param event StructureGrowEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onStructureGrow(final StructureGrowEvent event) {
        String type = "tree-grow";
        final TreeType species = event.getSpecies();
        if (species.name().toLowerCase().contains("mushroom")) {
            type = "mushroom-grow";
        }
        if (!Prism.getIgnore().event(type, event.getWorld())) {
            return;
        }
        for (final BlockState block : event.getBlocks()) {
            if (Utilities.isGrowableStructure(block.getType())) {
                if (event.getPlayer() != null) {
                    RecordingQueue.addToQueue(ActionFactory.createGrow(type, block, event.getPlayer()));
                } else {
                    RecordingQueue.addToQueue(ActionFactory.createGrow(type, block, "Environment"));
                }
            }
        }
    }

    /**
     * WorldLoadEvent.
     * @param event WorldLoadEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onWorldLoad(final WorldLoadEvent event) {

        final String worldName = event.getWorld().getName();

        if (!Prism.prismWorlds.containsKey(worldName)) {
            Prism.getPrismDataSource().addWorldName(worldName);
        }
    }

    /**
     * Track portal creation events.
     * @param event PortalCreateEvent.
     */
    @EventHandler(priority = EventPriority.MONITOR,ignoreCancelled = true)
    public void onPortalCreate(final PortalCreateEvent event) {
        String type = "portal-create";
        if (!Prism.getIgnore().event(type, event.getWorld())) {
            return;
        }
        for (final BlockState block : event.getBlocks()) {
            if (Utilities.isGrowableStructure(block.getType())) {
                Entity e = event.getEntity();
                if (e != null) {
                    if (e instanceof Player) {
                        RecordingQueue.addToQueue(ActionFactory.createGrow(type, block, (Player) event.getEntity()));
                    } else {
                        RecordingQueue.addToQueue(ActionFactory
                                .createGrow(type, block, event.getEntity().getName().toLowerCase()));
                    }
                } else {
                    RecordingQueue.addToQueue(ActionFactory.createGrow(type, block, "Environment"));
                }
            }
        }


    }
}