package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.ItemStackAction;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Chest;
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
	    
	    // Chest
	    if(ih instanceof Chest) {
		    Chest eventChest = (Chest) ih;
		    containerLoc = eventChest.getLocation();
	    }
	    
	    // Double chest
	    else if(ih instanceof DoubleChest) {
	    	DoubleChest eventChest = (DoubleChest) ih;
	    	containerLoc = eventChest.getLocation();
	    }
	    
	    // Furnace
	    else if(ih instanceof Furnace) {
	    	Furnace furnace = (Furnace) ih;
	    	containerLoc = furnace.getLocation();
	    }
	    
//	    // Dispenser
//	    else if(ih instanceof Dispenser) {
//	    	Dispenser dispenser = (Dispenser) ih;
//	    	containerLoc = dispenser.getLocation();
//		    
//	    	// still tracks player inv
//	    	plugin.debug("SLOT: " + event.getSlot());
//	    	plugin.debug("RAW SLOT: " + event.getRawSlot());
//		    
//	    }
	    
	    // We don't need to record this since enderchests are for the player only.
	    
	    if(containerLoc != null && event.getSlot() == event.getRawSlot()){
		    if(!event.getCurrentItem().getType().equals(Material.AIR)){
		    	plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_REMOVE, event.getCurrentItem(), containerLoc, player) );
		    }
		    if(!event.getCursor().getType().equals(Material.AIR)){
		    	plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_INSERT, event.getCursor(), containerLoc, player) );
		    }
	    }
	}
}