package me.botsko.prism.listeners;

import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionsQuery;
import me.botsko.prism.actions.QueryParameters;

import org.bukkit.ChatColor;
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
				
				// Build params
				QueryParameters params = new QueryParameters();
				params.setWorld( player.getWorld().getName() );
				params.setLoc(loc);
				
				// Query
				ActionsQuery aq = new ActionsQuery(plugin);
    			List<me.botsko.prism.actions.Action> results = aq.lookup( params );
    			if(!results.isEmpty()){
    				player.sendMessage( plugin.playerHeaderMsg("Recent history for this block:") );
    				for(me.botsko.prism.actions.Action a : results){
    					
    					// user
    					String msg = ChatColor.BLUE + a.getPlayer_name();
    					msg += " " + ChatColor.GRAY + a.getAction_type();
    					msg += " " + ChatColor.RED + a.getData();
    					msg += " " + ChatColor.RED + (int)a.getX();
    					msg += " " + ChatColor.RED + (int)a.getY();
    					msg += " " + ChatColor.RED + (int)a.getZ();
    					
    					player.sendMessage( plugin.playerMsg( msg ) );
    				}
    			} else {
    				player.sendMessage( plugin.playerError( "No results found." ) );
    			}
			
				event.setCancelled(true);
					
			}
			
			// Player right click on block, get last action
			if (event.getAction() == Action.RIGHT_CLICK_BLOCK) {
	
				Location loc = event.getClickedBlock().getLocation();
				plugin.debug("Running last action search for " + loc.getBlockX() + " " + loc.getBlockY() + " " + loc.getBlockZ());
				
				// Build params
				QueryParameters params = new QueryParameters();
				params.setWorld( player.getWorld().getName() );
				params.setLoc(loc);
				params.setLimit(1);
				
				// Query
				ActionsQuery aq = new ActionsQuery(plugin);
    			List<me.botsko.prism.actions.Action> results = aq.lookup( params );
    			if(!results.isEmpty()){
    				player.sendMessage( plugin.playerHeaderMsg("Last action of this block:") );
    				for(me.botsko.prism.actions.Action a : results){
    					
    					// user
    					String msg = ChatColor.BLUE + a.getPlayer_name();
    					msg += " " + ChatColor.GRAY + a.getAction_type();
    					msg += " " + ChatColor.RED + a.getData();
    					msg += " " + ChatColor.RED + (int)a.getX();
    					msg += " " + ChatColor.RED + (int)a.getY();
    					msg += " " + ChatColor.RED + (int)a.getZ();
    					
    					player.sendMessage( plugin.playerMsg( msg ) );
    				}
    			} else {
    				player.sendMessage( plugin.playerError( "No results found." ) );
    			}
				
				event.setCancelled(true);
				
			}
		}
	}
}
