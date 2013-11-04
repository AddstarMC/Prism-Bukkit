package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.BlockState;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryMoveItemEvent;
import org.bukkit.event.inventory.InventoryPickupItemEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;

public class PrismInventoryEvents implements Listener {
	
	/**
	 * 
	 */
	private Prism plugin;

	/**
	 * 
	 */
	private InventoryClickEvent event;
	
	/**
	 * 
	 */
	private Player player;
	
	/**
	 * 
	 */
	private Location containerLoc;
	
	/**
	 * 
	 */
	private ItemStack currentitem;
	
	/**
	 * 
	 */
	private ItemStack cursoritem;
	
	
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
		
		if( !plugin.getConfig().getBoolean("prism.track-item-events") ) return; // MCPC+ - renamed to represent more containers with inventory
		
		if( !Prism.getIgnore().event("item-pickup") ) return;
		
		// MCPC+ - removed hardcoded hopper check and added call for inventory holder name
		Prism.actionsRecorder.addToQueue( ActionFactory.create("item-pickup", event.getItem().getItemStack(), event.getItem().getItemStack().getAmount(), -1, null, event.getItem().getLocation(), event.getInventory().getName()));
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onInventoryMoveItem(final InventoryMoveItemEvent event){
		
		if( !plugin.getConfig().getBoolean("prism.track-item-events") ) return; // MCPC+ - renamed to represent more containers with inventory
		
		if( !Prism.getIgnore().event("item-insert") ) return;
		
		if( event.getDestination() == null ) return;
		
		// Get container
		InventoryHolder ih = event.getDestination().getHolder();
		Location containerLoc = null;
		if(ih instanceof BlockState){
			BlockState eventChest = (BlockState) ih;
		    containerLoc = eventChest.getLocation();
		}
		
		if( containerLoc == null ) return;
		
		// MCPC+ - removed hardcoded hopper check and added call for inventory holder name
		Prism.actionsRecorder.addToQueue( ActionFactory.create("item-insert", event.getItem(), event.getItem().getAmount(), 0, null, containerLoc, event.getDestination().getName()) );
	}
	
	
	/**
	 * Handle inventory transfers
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onInventoryClick(final InventoryClickEvent event) {
		
		if( !plugin.getConfig().getBoolean("prism.tracking.item-insert")
				&& !plugin.getConfig().getBoolean("prism.tracking.item-remove")) return;
		
		// Store some info
		player = (Player) event.getWhoClicked();
		this.event = event;
	    currentitem = event.getCurrentItem();
	    cursoritem = event.getCursor();

	    // Get location
	    if( event.getInventory().getHolder() instanceof BlockState ){
	    	BlockState b = (BlockState) event.getInventory().getHolder();
	    	containerLoc = b.getLocation();
	    }
	    else if( event.getInventory().getHolder() instanceof Entity ){
	    	Entity e = (Entity) event.getInventory().getHolder();
	    	containerLoc = e.getLocation();
	    }
	    
//	    Prism.debug("Raw slot: " + event.getRawSlot());
//	    Prism.debug("Slot: " + event.getSlot());
//	    Prism.debug("Def. Size " + event.getView().getType().getDefaultSize());
//	    Prism.debug("Cursor Item: " + (cursoritem != null ? cursoritem.getTypeId() : "null"));
//	    Prism.debug("Current Item: " + (currentitem != null ? currentitem.getTypeId() : "null"));
	    
    	if( event.getSlot() == event.getRawSlot() && event.getRawSlot() <= event.getView().getType().getDefaultSize() ){
    		
    		// If BOTH items are not air then you've swapped an item. We need to record an insert for the cursor item and
    		// and remove for the current.
    		if( currentitem != null && !currentitem.getType().equals(Material.AIR) && cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
    			recordInvAction( player, currentitem, event.getRawSlot(), "item-remove");
    			recordInvAction( player, cursoritem, event.getRawSlot(), "item-insert");
    		}
    		else if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
		    	recordInvAction( player, currentitem, event.getRawSlot(), "item-remove");
		    }
    		else if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
		    	recordInvAction( player, cursoritem, event.getRawSlot(), "item-insert");
		    }
    		return;
    	}
    	if( event.isShiftClick() && cursoritem != null && cursoritem.getType().equals(Material.AIR) ){
    		recordInvAction( player, currentitem, -1, "item-insert");
    	}
	}
	
	
	/**
	 *
	 * @param player
	 * @param item
	 * @param slot
	 * @param actionType
	 */
	protected void recordInvAction( Player player, ItemStack item, int slot, String actionType ){
		
		if( !Prism.getIgnore().event(actionType,player) ) return;
		
		// Determine correct quantity. Right-click events change the item
	    // quantity but don't seem to update the cursor/current items.
	    int officialQuantity = 0;
	    if(item != null){
		    officialQuantity = item.getAmount();
		    // If the player right-clicked we need to assume the amount
		    if( event.isRightClick() ){
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
		    Prism.actionsRecorder.addToQueue( ActionFactory.create(actionType, item, officialQuantity, slot, null, containerLoc, player.getName()) );
	    }
	}
}