package me.botsko.prism.listeners;

import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionsQuery;
import me.botsko.prism.actions.QueryParameters;

import org.bukkit.ChatColor;
import org.bukkit.Location;
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
		player.sendMessage( plugin.playerHeaderMsg("Recent history for this " + block.getType().toString().toLowerCase() + " block: " ) );
		
		// Build params
		QueryParameters params = new QueryParameters();
		params.setWorld( player.getWorld().getName() );
		params.setLoc(loc);
		
		// Query
		ActionsQuery aq = new ActionsQuery(plugin);
		List<me.botsko.prism.actions.Action> results = aq.lookup( params );
		if(!results.isEmpty()){
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
	}
}
