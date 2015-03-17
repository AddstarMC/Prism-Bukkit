package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.BlockState;
import org.bukkit.block.DoubleChest;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.inventory.InventoryPickupItemEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;

import java.util.Map;
import java.util.Map.Entry;

public class PrismInventoryEvents implements Listener {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     */
    public PrismInventoryEvents(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onInventoryPickupItem(final InventoryPickupItemEvent event) {

        if( !plugin.getConfig().getBoolean( "prism.track-hopper-item-events" ) )
            return;

        if( !Prism.getIgnore().event( "item-pickup" ) )
            return;

        // If hopper
        if( event.getInventory().getType().equals( InventoryType.HOPPER ) ) {
            RecordingQueue.addToQueue( ActionFactory.createItemStack("item-pickup", event.getItem().getItemStack(), event
                    .getItem().getItemStack().getAmount(), -1, null, event.getItem().getLocation(), "hopper") );
        }
    }

    /**
     * Handle inventory transfers
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onInventoryDrag(final InventoryDragEvent event) {

        if( !plugin.getConfig().getBoolean( "prism.tracking.item-insert" )
                && !plugin.getConfig().getBoolean( "prism.tracking.item-remove" ) )
            return;

        // Get container
        final InventoryHolder ih = event.getInventory().getHolder();
        Location containerLoc = null;
        if( ih instanceof BlockState ) {
            final BlockState eventChest = (BlockState) ih;
            containerLoc = eventChest.getLocation();
        }

        // Store some info
        final Player player = (Player) event.getWhoClicked();

        final Map<Integer, ItemStack> newItems = event.getNewItems();
        for ( final Entry<Integer, ItemStack> entry : newItems.entrySet() ) {
            recordInvAction( player, containerLoc, entry.getValue(), entry.getKey(), "item-insert" );
        }
    }

    /**
     * Handle inventory transfers
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onInventoryClick(final InventoryClickEvent event) {

        if( !plugin.getConfig().getBoolean( "prism.tracking.item-insert" )
                && !plugin.getConfig().getBoolean( "prism.tracking.item-remove" ) )
            return;

        Location containerLoc = null;

        // Store some info
        final Player player = (Player) event.getWhoClicked();
        final ItemStack currentitem = event.getCurrentItem();
        final ItemStack cursoritem = event.getCursor();

        // Get location
        if( event.getInventory().getHolder() instanceof BlockState ) {
            final BlockState b = (BlockState) event.getInventory().getHolder();
            containerLoc = b.getLocation();
        } else if( event.getInventory().getHolder() instanceof Entity ) {
            final Entity e = (Entity) event.getInventory().getHolder();
            containerLoc = e.getLocation();
        } else if( event.getInventory().getHolder() instanceof DoubleChest ) {
            final DoubleChest chest = (DoubleChest) event.getInventory().getHolder();
            containerLoc = chest.getLocation();
        }
        
        // Double chests report 27 default size, though they actually
        // have 6 rows of 9 for 54 slots
        int defaultSize = event.getView().getType().getDefaultSize();
        if( event.getInventory().getHolder() instanceof DoubleChest ){
            defaultSize = event.getView().getType().getDefaultSize() * 2;
        }

        // Click in the block inventory produces slot/rawslot that are equal, only until the slot numbers exceed the
        // slot count of the inventory. At that point, they represent the player inv.
        if( event.getSlot() == event.getRawSlot() && event.getRawSlot() <= defaultSize ) {
            ItemStack addStack = null;
            ItemStack removeStack = null;

            if( currentitem != null && !currentitem.getType().equals( Material.AIR ) && cursoritem != null
                    && !cursoritem.getType().equals( Material.AIR ) ) {
                // If BOTH items are not air then you've swapped an item. We need to
                // record an insert for the cursor item and
                // and remove for the current.

                if (currentitem.isSimilar(cursoritem)) {
                    // Items are similar enough to stack
                    int amount = cursoritem.getAmount();

                    if(event.isRightClick()) {
                        amount = 1;
                    }

                    int remaining = (currentitem.getMaxStackSize() - currentitem.getAmount());
                    int inserted = (amount <= remaining) ? amount : remaining;

                    if (inserted > 0) {
                        addStack = cursoritem.clone();
                        addStack.setAmount(inserted);
                    }
                } else {
                    // Items are not similar
                    addStack = cursoritem.clone();
                    removeStack = currentitem.clone();
                }
            } else if( currentitem != null && !currentitem.getType().equals( Material.AIR ) ) {
                removeStack = currentitem.clone();
            } else if( cursoritem != null && !cursoritem.getType().equals( Material.AIR ) ) {
                addStack = cursoritem.clone();
            }

            // Record events
            if (addStack != null) {
                recordInvAction( player, containerLoc, addStack, event.getRawSlot(), "item-insert", event );
            }
            if (removeStack != null) {
                recordInvAction( player, containerLoc, removeStack, event.getRawSlot(), "item-remove", event );
            }
            return;
        }
        if( event.isShiftClick() && cursoritem != null && cursoritem.getType().equals( Material.AIR ) ) {
            recordInvAction( player, containerLoc, currentitem, -1, "item-insert", event );
        }
    }

    /**
     * 
     * @param player
     * @param item
     * @param slot
     * @param actionType
     */
    protected void recordInvAction(Player player, Location containerLoc, ItemStack item, int slot, String actionType) {
        recordInvAction( player, containerLoc, item, slot, actionType, null );
    }

    /**
     * 
     * @param player
     * @param item
     * @param slot
     * @param actionType
     */
    protected void recordInvAction(Player player, Location containerLoc, ItemStack item, int slot, String actionType,
            InventoryClickEvent event) {
        if( !Prism.getIgnore().event( actionType, player ) )
            return;

        // Determine correct quantity. Right-click events change the item
        // quantity but don't seem to update the cursor/current items.
        int officialQuantity = 0;
        if( item != null ) {
            officialQuantity = item.getAmount();
            // If the player right-clicked we need to assume the amount
            if( event != null && event.isRightClick() ) {
                // If you're right-clicking to remove an item, it divides by two
                if( actionType.equals( "item-remove" ) ) {
                    officialQuantity = ( officialQuantity - (int) Math.floor( ( item.getAmount() / 2 ) ) );
                }
                // If you're right-clicking to insert, it's only one
                else if( actionType.equals( "item-insert" ) ) {
                    officialQuantity = 1;
                }
            }
        }

        // Record it!
        if( actionType != null && containerLoc != null && item != null && item.getTypeId() != 0 && officialQuantity > 0 ) {
            RecordingQueue.addToQueue( ActionFactory.createItemStack(actionType, item, officialQuantity, slot, null,
                    containerLoc, player.getName()) );
        }
    }
}
