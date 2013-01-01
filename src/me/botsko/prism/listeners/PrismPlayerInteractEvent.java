package me.botsko.prism.listeners;

import me.botsko.prism.Prism;

import org.bukkit.Location;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerInteractEvent;

public class PrismPlayerInteractEvent implements Listener {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public PrismPlayerInteractEvent( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.HIGHEST)
	public void onPlayerInteract(PlayerInteractEvent event) {
		
		Player player = event.getPlayer();
		
		if(plugin.playersWithActiveTools.contains(player.getName())){
		
			// Player left click on block, run a history search
			if (event.getAction() == Action.LEFT_CLICK_BLOCK) {
	
				Location loc = event.getClickedBlock().getLocation();
				plugin.debug("Running history search for " + loc.getBlockX() + " " + loc.getBlockY() + " " + loc.getBlockZ());
			
				event.setCancelled(true);
					
			}
			
			// Player right click on block, get last action
			if (event.getAction() == Action.RIGHT_CLICK_BLOCK) {
	
				Location loc = event.getClickedBlock().getLocation();
				plugin.debug("Running last action search for " + loc.getBlockX() + " " + loc.getBlockY() + " " + loc.getBlockZ());
				
				event.setCancelled(true);
				
			}
		}
	}
}
