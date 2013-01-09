package me.botsko.prism.listeners.self;

import me.botsko.prism.Prism;
import me.botsko.prism.events.PrismBlockReplaceEvent;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

public class PrismBlockEventListener implements Listener {
	
	/**
	 * 
	 */
	private Prism plugin;

	
	/**
	 * 
	 * @param plugin
	 */
	public PrismBlockEventListener( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler
	public void onPrismBlockReplaceEvent(PrismBlockReplaceEvent event){
		plugin.debug("---- PrismBlockReplaceEvent: " + event.getOnBehalfOf());
		plugin.debug("Replaced block due to rollback/restore. Changed " + event.getOriginalBlock().getTypeId() + " to " + event.getNewBlock().getTypeId());
	}
	
	// PrismBlockReplaceEvent - When a replace a block in the world
	// PrismBlockRemoveEvent - When we remove a block from the world
	
}
