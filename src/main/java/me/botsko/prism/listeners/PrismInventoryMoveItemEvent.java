package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;

import org.bukkit.Location;
import org.bukkit.block.BlockState;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryMoveItemEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.InventoryHolder;

public class PrismInventoryMoveItemEvent implements Listener {

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onInventoryMoveItem(final InventoryMoveItemEvent event) {

        if( !Prism.getIgnore().event( "item-insert" ) )
            return;

        if( event.getDestination() == null )
            return;

        // Get container
        final InventoryHolder ih = event.getDestination().getHolder();
        Location containerLoc = null;
        if( ih instanceof BlockState ) {
            final BlockState eventChest = (BlockState) ih;
            containerLoc = eventChest.getLocation();
        }

        if( containerLoc == null )
            return;

        if( event.getSource().getType().equals( InventoryType.HOPPER ) ) {
            RecordingQueue.addToQueue( ActionFactory.createItemStack("item-insert", event.getItem(), event.getItem()
                    .getAmount(), 0, null, containerLoc, "hopper") );
        }
    }
}