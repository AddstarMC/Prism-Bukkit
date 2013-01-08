package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.ItemStackAction;

import org.bukkit.Location;
import org.bukkit.Material;
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
		
		Inventory inv = event.getInventory();
		Player player = (Player) event.getWhoClicked();
	    

		Location containerLoc = null;
	    InventoryHolder ih = inv.getHolder();
	    ItemStack currentitem = event.getCurrentItem();
	    ItemStack cursoritem = event.getCursor();
	    ActionType actionType = null;
	    ItemStack item = null;
	    

	    // Chest
	    // If the slot type is <= 35 and rawslot >= 36, it means we're shift+clicking an item into
	    // the chest.
	    if(ih instanceof Chest) {
	    	Chest eventChest = (Chest) ih;
		    containerLoc = eventChest.getLocation();
	    	if( event.getSlot() == event.getRawSlot() ){
	    		if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
			    	item = currentitem;
			    	actionType = ActionType.ITEM_REMOVE;
			    }
			    if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
			    	item = cursoritem;
			    	actionType = ActionType.ITEM_INSERT;
			    }
	    	}
	    	// this triggers whether it's a shift click or not
	    	if( event.isShiftClick() && event.getSlot() <= 35 && event.getRawSlot() >= 36 && cursoritem != null && cursoritem.getType().equals(Material.AIR) ){
	    		actionType = ActionType.ITEM_INSERT;
	    		item = currentitem;
	    	}
	    }
	    
	    // Double chest
	    else if(ih instanceof DoubleChest) {
	    	DoubleChest eventChest = (DoubleChest) ih;
	    	containerLoc = eventChest.getLocation();
	    	if( event.getSlot() == event.getRawSlot() ){
	    		if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
			    	item = currentitem;
			    	actionType = ActionType.ITEM_REMOVE;
			    }
			    if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
			    	item = cursoritem;
			    	actionType = ActionType.ITEM_INSERT;
			    }
	    	}
	    	if( event.isShiftClick() && event.getSlot() <= 35 && event.getRawSlot() >= 36 && cursoritem != null && cursoritem.getType().equals(Material.AIR) ){
	    		actionType = ActionType.ITEM_INSERT;
	    		item = currentitem;
	    	}
	    }
	    
	    // Furnace
	    else if(ih instanceof Furnace) {
	    	if(event.getSlot() == event.getRawSlot()){
		    	Furnace furnace = (Furnace) ih;
		    	containerLoc = furnace.getLocation();
		    	if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
			    	item = currentitem;
			    	actionType = ActionType.ITEM_REMOVE;
			    }
			    if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
			    	item = cursoritem;
			    	actionType = ActionType.ITEM_INSERT;
			    }
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
			    	item = currentitem;
			    	actionType = ActionType.ITEM_REMOVE;
			    }
			    if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
			    	item = cursoritem;
			    	actionType = ActionType.ITEM_INSERT;
			    }
			} else {
				// Otherwise the player has to be clicking in their inventory. We'd record the insert
				// if they manually drag the item in, but we have to watch for sneaky shift+clicks.
				if( event.isShiftClick() ){
					actionType = ActionType.ITEM_INSERT;
		    		item = currentitem;
				}
			}
		}
	    

	    // Determine correct quantity. Right-click events change the item
	    // quantity but don't seem to update the cursor/current items.
	    int officialQuantity = 0;
	    if(item != null){
		    officialQuantity = item.getAmount();
		    // If the player right-clicked we need to assume the amount
		    if( event.isRightClick() ){
		    	// If you're right-clicking to remove an item, it divides by two
		    	if( actionType.equals(ActionType.ITEM_REMOVE) ){
		    		officialQuantity = (officialQuantity - (int)Math.floor( (item.getAmount() / 2) ));
		    	}
		    	// If you're right-clicking to insert, it's only one
		    	else if( actionType.equals(ActionType.ITEM_INSERT) ){
		    		officialQuantity = 1;
		    	}
		    }
	    }
	    
	    // Record it!
	    if(actionType != null && containerLoc != null && item != null){
		    plugin.actionsRecorder.addToQueue( new ItemStackAction(actionType, item, officialQuantity, containerLoc, player.getName()) );
	    }
	}
}