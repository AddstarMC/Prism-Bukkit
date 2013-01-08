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
import org.bukkit.event.inventory.InventoryType.SlotType;
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
	    

	    // Ignore player slow types
		SlotType slotType = event.getSlotType();
		if ( !slotType.equals(SlotType.CONTAINER) && !slotType.equals(SlotType.FUEL) && !slotType.equals(SlotType.RESULT) ){
			return;
		}

		Location containerLoc = null;
	    InventoryHolder ih = inv.getHolder();
	    boolean should_catch = false;
	    
	    // Chest
	    if(ih instanceof Chest) {
	    	if(event.getSlot() == event.getRawSlot()){
			    Chest eventChest = (Chest) ih;
			    containerLoc = eventChest.getLocation();
			    should_catch = true;
	    	}
	    }
	    
	    // Double chest
	    else if(ih instanceof DoubleChest) {
	    	if(event.getSlot() == event.getRawSlot()){
		    	DoubleChest eventChest = (DoubleChest) ih;
		    	containerLoc = eventChest.getLocation();
		    	should_catch = true;
	    	}
	    }
	    
	    // Furnace
	    else if(ih instanceof Furnace) {
	    	if(event.getSlot() == event.getRawSlot()){
		    	Furnace furnace = (Furnace) ih;
		    	containerLoc = furnace.getLocation();
		    	should_catch = true;
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
				should_catch = true;
			}
		}
	    
	    // We don't need to record this since enderchests are for the player only.
	    
	    if(should_catch && containerLoc != null){
	    	ItemStack currentitem = event.getCurrentItem();
		    if( currentitem != null && !currentitem.getType().equals(Material.AIR) ){
		    	plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_REMOVE, currentitem, containerLoc, player.getName()) );
		    }
		    ItemStack cursoritem = event.getCursor();
		    if( cursoritem != null && !cursoritem.getType().equals(Material.AIR) ){
		    	plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_INSERT, cursoritem, containerLoc, player.getName()) );
		    }
	    }
	}
}