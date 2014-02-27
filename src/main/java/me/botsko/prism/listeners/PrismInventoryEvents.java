package me.botsko.prism.listeners;

import java.util.Map;
import java.util.Map.Entry;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.block.DoubleChest;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.inventory.InventoryOpenEvent;
import org.bukkit.event.inventory.InventoryPickupItemEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;

public class PrismInventoryEvents implements Listener {
	
	/**
	 * 
	 */
	private final Prism plugin;

	
	/**
	 * 
	 * @param plugin
	 */
	public PrismInventoryEvents( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onInventoryPickupItem(final InventoryPickupItemEvent event){
		
		if( !plugin.getConfig().getBoolean("prism.track-hopper-item-events") ) return;
		
		if( !Prism.getIgnore().event("item-pickup") ) return;
		
		// If hopper
		if( event.getInventory().getType().equals(InventoryType.HOPPER) ){
			RecordingQueue.addToQueue( ActionFactory.create("item-pickup", event.getItem().getItemStack(), event.getItem().getItemStack().getAmount(), -1, null, event.getItem().getLocation(), "hopper") );
		}
	}
	
	
	/**
	 * Handle inventory transfers
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
	public void onInventoryDrag(final InventoryDragEvent event) {
		
		// Someone cancelled this before we did 
		if ( event.isCancelled() ) {
			return;
		}
		
		if( !plugin.getConfig().getBoolean("prism.tracking.item-insert")
				&& !plugin.getConfig().getBoolean("prism.tracking.item-remove")) return;
		
		// Get container
		InventoryHolder ih = event.getInventory().getHolder();
		Location containerLoc = null;
		if(ih instanceof BlockState){
			BlockState eventChest = (BlockState) ih;
		    containerLoc = eventChest.getLocation();
		}
		
		// Store some info
		Player player = (Player) event.getWhoClicked();
	    
	    Map<Integer,ItemStack> newItems = event.getNewItems();
	    for (Entry<Integer, ItemStack> entry : newItems.entrySet()) {
	    	recordInvAction( player, containerLoc, entry.getValue(), entry.getKey(), false, "item-insert");
	    }
	}
	
	
	/**
	 * Handle inventory transfers
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onInventoryClick(final InventoryClickEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.item-insert")
				&& !plugin.getConfig().getBoolean("prism.tracking.item-remove")) return;
		
		Location containerLoc = null;

		// Store some info
		Player player = (Player) event.getWhoClicked();

		ItemStack currentitem = event.getCurrentItem();
		ItemStack cursoritem = event.getCursor();

		// Get location
		if( event.getInventory().getHolder() instanceof BlockState ){
			BlockState b = (BlockState) event.getInventory().getHolder();
			containerLoc = b.getLocation();
		}
		else if( event.getInventory().getHolder() instanceof Entity ){
			Entity e = (Entity) event.getInventory().getHolder();
			containerLoc = e.getLocation();
		}
		else if( event.getInventory().getHolder() instanceof DoubleChest ){
			DoubleChest chest = (DoubleChest) event.getInventory().getHolder();
			containerLoc = chest.getLocation();
		}
	    
//	    Prism.debug("--- Inv: " + (inv == null ? "null" : inv.toString()));
//	    Prism.debug("--- InvHold: " + (ih == null ? "null" : ih.toString()));
//	    Prism.debug("Raw slot: " + event.getRawSlot());
//	    Prism.debug("Slot: " + event.getSlot());
//	    Prism.debug("Def. Size " + event.getView().getType().getDefaultSize());
//	    Prism.debug("Cursor Item: " + (cursoritem != null ? cursoritem.getTypeId() : "null"));
//	    Prism.debug("Current Item: " + (currentitem != null ? currentitem.getTypeId() : "null"));

		if( event.getSlot() == event.getRawSlot()) { //&& event.getRawSlot() <= event.getView().getType().getDefaultSize() ){
			// If BOTH items are not air then you've swapped an item. We need to record an insert for the cursor item and
			// and remove for the current.
			if( currentitem != null && !currentitem.getType().equals(Material.AIR) && cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
				recordInvAction( player, containerLoc, currentitem, event.getRawSlot(), event.isRightClick(), "item-remove");
				recordInvAction( player, containerLoc, cursoritem, event.getRawSlot(), event.isRightClick(), "item-insert");
			}
			else if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
				recordInvAction( player, containerLoc, currentitem, event.getRawSlot(), event.isRightClick(), "item-remove");
			}
			else if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
				recordInvAction( player, containerLoc, cursoritem, event.getRawSlot(), event.isRightClick(), "item-insert");
			}
			return;
		}
		if( event.isShiftClick() && cursoritem != null && cursoritem.getType().equals(Material.AIR) ){
			recordInvAction( player, containerLoc, currentitem, -1, event.isRightClick(), "item-insert");
		}
	}

    /**
     * Handle container access
     * @param event
     */
    @EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
    public void onInventoryOpen(final InventoryOpenEvent event) {

        Location containerLoc = null;

        // Store some info
        Player player = (Player) event.getPlayer();

        Block block = null;
        if (event.getInventory().getHolder() != null && event.getInventory().getHolder() instanceof BlockState)
        {
            BlockState state = (BlockState)event.getInventory().getHolder();
            block = state.getBlock();
            containerLoc = block.getLocation();
        }
        
        // MCPC+ start - handle container access
        if( !Prism.getIgnore().event("container-open",player) || block == null ) return;
        	RecordingQueue.addToQueue( ActionFactory.create("container-open", block, containerLoc, player != null ? player.getName() : "") );
        // MCPC+ end
    }

    /**
     * Handle inventory transfers
     * @param event
     */
    @EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
    public void onInventoryClose(final InventoryCloseEvent event) {

        Location containerLoc = null;

        // Store some info
        Player player = (Player) event.getPlayer();

        Block block = null;
        if (event.getInventory().getHolder() != null && event.getInventory().getHolder() instanceof BlockState)
        {
            BlockState state = (BlockState)event.getInventory().getHolder();
            block = state.getBlock();
            containerLoc = block.getLocation();
        }
        
        // MCPC+ start - handle container access
        if( !Prism.getIgnore().event("container-close",player) || block == null ) return;
        	RecordingQueue.addToQueue( ActionFactory.create("container-close", block, containerLoc, player != null ? player.getName() : "") );
        // MCPC+ end
    }
	
	/**
	 *
     * @param player
	 * @param item
     * @param slot
	 * @param actionType
	 */
	protected void recordInvAction( Player player, Location containerLoc, ItemStack item, int slot, boolean isRightClick, String actionType){
		
		if( !Prism.getIgnore().event(actionType,player) ) return;
		
		// Determine correct quantity. Right-click events change the item
	    // quantity but don't seem to update the cursor/current items.
	    int officialQuantity = 0;
	    if(item != null){
		    officialQuantity = item.getAmount();
		    // If the player right-clicked we need to assume the amount
		    if( isRightClick ){
		    	// If you're right-clicking to remove an item, it divides by two
		    	if( actionType.equals("item-remove") ){
		    		officialQuantity = (officialQuantity - (int)Math.floor( (item.getAmount() / 2) ));
		    	}
		    	// If you're right-clicking to insert, it's only one
		    	else if( actionType.equals("item-insert") ){
		    		officialQuantity = 1;
		    	}
		    }
	    }
	    // Record it!
	    if(actionType != null && containerLoc != null && item != null && item.getTypeId() != 0 && officialQuantity > 0){
		    RecordingQueue.addToQueue( ActionFactory.create(actionType, item, officialQuantity, slot, null, containerLoc, player.getName()) );
	    }
	}
}