package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ItemStackAction;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.BrewingStand;
import org.bukkit.block.Chest;
import org.bukkit.block.Dispenser;
import org.bukkit.block.DoubleChest;
import org.bukkit.block.Furnace;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.inventory.Inventory;
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
	private InventoryHolder ih;
	
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
	 * Handle inventory transfers
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
	public void onInventoryClick(final InventoryClickEvent event) {
		
		// Someone cancelled this before we did 
		if ( event.isCancelled() ) {
			return;
		}
		
		
		if( !plugin.getConfig().getBoolean("prism.tracking.item-insert")
				&& !plugin.getConfig().getBoolean("prism.tracking.item-remove")) return;
		
		// Store some info
		Inventory inv = event.getInventory();
		player = (Player) event.getWhoClicked();
		this.event = event;
	    ih = inv.getHolder();
	    currentitem = event.getCurrentItem();
	    cursoritem = event.getCursor();
	    
//	    plugin.debug("Raw slot: " + event.getRawSlot());
//	    plugin.debug("Slot: " + event.getSlot());
//	    plugin.debug("Cursor Item: " + (cursoritem != null ? cursoritem.getTypeId() : "null"));
//	    plugin.debug("Current Item: " + (currentitem != null ? currentitem.getTypeId() : "null"));
	    

	    // Chest
	    // If the slot type is <= 35 and rawslot >= 36, it means we're shift+clicking an item into
	    // the chest.
	    if(ih instanceof Chest) {
	    	Chest eventChest = (Chest) ih;
		    containerLoc = eventChest.getLocation();
	    	if( event.getSlot() == event.getRawSlot() ){
	    		
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
	    	if( event.isShiftClick() && event.getSlot() <= 35 && event.getRawSlot() >= 36 && cursoritem != null && cursoritem.getType().equals(Material.AIR) ){
	    		recordInvAction( player, currentitem, -1, "item-insert");
	    	}
	    }
	    
	    // Double chest
	    else if(ih instanceof DoubleChest) {
	    	DoubleChest eventChest = (DoubleChest) ih;
	    	containerLoc = eventChest.getLocation();
	    	if( event.getSlot() == event.getRawSlot() ){
	    		if( currentitem != null && !currentitem.getType().equals(Material.AIR) && cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
	    			recordInvAction( player, currentitem, event.getRawSlot(), "item-remove");
	    		}
	    		else if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
	    			recordInvAction( player, currentitem, event.getRawSlot(), "item-remove");
			    }
			    if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
			    	recordInvAction( player, cursoritem, event.getRawSlot(), "item-insert");
			    }
			    return;
	    	}
	    	if( event.isShiftClick() && event.getSlot() <= 35 && event.getRawSlot() >= 36 && cursoritem != null && cursoritem.getType().equals(Material.AIR) ){
	    		recordInvAction( player, currentitem, -1, "item-insert");
	    	}
	    }
	    
	    // Furnace
	    else if(ih instanceof Furnace) {
	    	if(event.getSlot() == event.getRawSlot()){
		    	Furnace furnace = (Furnace) ih;
		    	containerLoc = furnace.getLocation();
		    	if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
		    		recordInvAction( player, currentitem, event.getRawSlot(), "item-remove");
			    }
			    if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
			    	recordInvAction( player, cursoritem, event.getRawSlot(), "item-insert");
			    }
			    return;
	    	}
	    	if( event.isShiftClick() ){
	    		recordInvAction( player, currentitem, -1, "item-insert");
	    	}
	    }
	    
		// Dispenser
	    // Took a bit of effort to figure.
	    // http://forums.bukkit.org/threads/excluding-player-inventory-clicks-when-using-dispenser.120495/
		else if(ih instanceof Dispenser) {
			Dispenser dispenser = (Dispenser) ih;
			containerLoc = dispenser.getLocation();
			
			// Only a click in the dispenser can trigger a slot < 9
			if(event.getRawSlot() <= 8){
				if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
					recordInvAction( player, currentitem, event.getRawSlot(), "item-remove");
			    }
			    if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
			    	recordInvAction( player, cursoritem, event.getRawSlot(), "item-insert");
			    }
			} else {
				// Otherwise the player has to be clicking in their inventory. We'd record the insert
				// if they manually drag the item in, but we have to watch for sneaky shift+clicks.
				if( event.isShiftClick() ){
		    		recordInvAction( player, currentitem, -1, "item-insert");
				}
			}
		}
	    
	    // Brewing stand
		else if(ih instanceof BrewingStand) {
			BrewingStand brewer = (BrewingStand) ih;
			containerLoc = brewer.getLocation();
			if( event.getSlot() == event.getRawSlot() ){
	    		if( currentitem != null && !currentitem.getType().equals(Material.AIR) && cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
	    			recordInvAction( player, currentitem, event.getRawSlot(), "item-remove");
	    		}
	    		else if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
	    			recordInvAction( player, currentitem, event.getRawSlot(), "item-remove");
			    }
			    if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
			    	recordInvAction( player, cursoritem, event.getRawSlot(), "item-insert");
			    }
			    return;
	    	}
			if( event.isShiftClick() ){
	    		recordInvAction( player, currentitem, -1, "item-insert");
	    	}
		}
	}
	
	
	/**
	 * 
	 * @param item
	 * @param actionType
	 * @param containerLoc
	 * @param event
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
		    Prism.actionsRecorder.addToQueue( new ItemStackAction(actionType, item, officialQuantity, slot, null, containerLoc, player.getName()) );
	    }
	}
}