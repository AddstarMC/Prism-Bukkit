package me.botsko.prism.listeners;

import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
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
		Block block = null;
		
		if(plugin.playersWithActiveTools.contains(player.getName())){
		
			// Player left click on block, run a history search
			if (event.getAction() == Action.LEFT_CLICK_BLOCK) {
				block = event.getClickedBlock();
			}
			// Player right click on block, get last action
			if (event.getAction() == Action.RIGHT_CLICK_BLOCK) {
				block = event.getClickedBlock().getRelative(event.getBlockFace());
			}
			if(block != null){
				showBlockHistory(player, block, block.getLocation());
				event.setCancelled(true);
			}
		}
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 * @param loc
	 */
	protected void showBlockHistory( Player player, Block block, Location loc ){
		
		plugin.debug("Running history search for " + loc.getBlockX() + " " + loc.getBlockY() + " " + loc.getBlockZ());

		// Build params
		QueryParameters params = new QueryParameters();
		params.setWorld( player.getWorld().getName() );
		params.setLoc(loc);
		
		// Query
		ActionsQuery aq = new ActionsQuery(plugin);
		List<me.botsko.prism.actions.Action> results = aq.lookup( params );
		if(!results.isEmpty()){
			for(me.botsko.prism.actions.Action a : results){
				ActionMessage am = new ActionMessage(a);
				player.sendMessage( plugin.playerHeaderMsg( am.getMessage() ) );
			}
		} else {
			String space_name = (block.getType().equals(Material.AIR) ? "space" : block.getType().toString().toLowerCase() + " block");
			player.sendMessage( plugin.playerError( "No history for this " + space_name + " found." ) );
		}
	}
}
