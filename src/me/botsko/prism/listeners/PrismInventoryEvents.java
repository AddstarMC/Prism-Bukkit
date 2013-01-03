package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.ItemStackAction;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Chest;
import org.bukkit.block.DoubleChest;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryType;
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

		// If this isn't a chest leave
		// @todo remove this. implement for anvils, dispensers, brewing stands, ender chests
	    if ( inv.getType() != InventoryType.CHEST ) {
	    	return;
	    }
	    
	    // If slot isn't a container leave
	    if ( event.getSlotType() != SlotType.CONTAINER ) {
	    	return;
	    }
	    
	    Location chestLoc = null;
	    InventoryHolder ih = event.getInventory().getHolder();
	    if(ih instanceof Chest) {
		    Chest eventChest = (Chest) ih;
		    chestLoc = eventChest.getLocation();
	    }
	    else if(ih instanceof DoubleChest) {
	    	DoubleChest eventChest = (DoubleChest) ih;
	    	chestLoc = eventChest.getLocation();
	    }

	    // Is item coming or going?
	    if(event.getSlot() == event.getRawSlot() && !event.getCursor().getType().equals(Material.AIR)){
	    	plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_INSERT, event.getCursor(), chestLoc, player) );
	    }
	    if(event.getSlot() == event.getRawSlot() && event.getCursor().getType().equals(Material.AIR)){
	    	plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_REMOVE, event.getCurrentItem(), chestLoc, player) );
	    }
	}
}