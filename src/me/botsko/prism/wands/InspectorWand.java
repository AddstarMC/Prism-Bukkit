package me.botsko.prism.wands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;

public class InspectorWand implements Wand {
	
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public InspectorWand( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 */
	@Override
	public void playerLeftClick(Player player, Block block) {
		if(block != null){
			showBlockHistory(player, block, block.getLocation());
		}
	}

	
	/**
	 * 
	 */
	@Override
	public void playerRightClick(Player player, Block block) {
		if(block != null){
			showBlockHistory(player, block, block.getLocation());
		}
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 * @param loc
	 */
	protected void showBlockHistory( Player player, Block block, Location loc ){

		// Build params
		QueryParameters params = new QueryParameters();
		params.setWorld( player.getWorld().getName() );
		params.setSpecificBlockLocation(loc);
		
		// Query
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( player, params );
		if(!results.getActionResults().isEmpty()){
			for(me.botsko.prism.actions.Action a : results.getActionResults()){
				ActionMessage am = new ActionMessage(a);
				player.sendMessage( plugin.playerHeaderMsg( am.getMessage() ) );
			}
		} else {
			String space_name = (block.getType().equals(Material.AIR) ? "space" : block.getType().toString().toLowerCase() + " block");
			player.sendMessage( plugin.playerError( "No history for this " + space_name + " found." ) );
		}
	}
}