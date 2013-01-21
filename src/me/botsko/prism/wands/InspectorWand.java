package me.botsko.prism.wands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.entity.Entity;
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
			String blockname = plugin.getItems().getItemStackAliasById(block.getTypeId(), block.getData());
			player.sendMessage( plugin.playerHeaderMsg( ChatColor.GOLD + "--- Inspecting "+blockname+" at "+loc.getBlockX()+" "+loc.getBlockY()+" "+loc.getBlockZ()+" ---" ) );
			if(results.getActionResults().size() > 5){
				player.sendMessage( plugin.playerHeaderMsg("Showing "+results.getTotal_results()+" results. Page 1 of "+results.getTotal_pages()) );
			}
			for(me.botsko.prism.actions.Action a : results.getPaginatedActionResults()){
				ActionMessage am = new ActionMessage(a);
				player.sendMessage( plugin.playerMsg( am.getMessage() ) );
			}
		} else {
			String space_name = (block.getType().equals(Material.AIR) ? "space" : block.getType().toString().toLowerCase() + " block");
			player.sendMessage( plugin.playerError( "No history for this " + space_name + " found." ) );
		}
	}


	/**
	 * 
	 */
	@Override
	public void playerRightClick(Player player, Entity entity) {
		return;
	}
}