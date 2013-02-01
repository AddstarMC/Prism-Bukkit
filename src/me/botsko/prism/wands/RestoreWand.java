package me.botsko.prism.wands;

import java.util.ArrayList;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.appliers.Restore;

public class RestoreWand implements Wand {

	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public RestoreWand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 */
	@Override
	public void playerLeftClick(Player player, Block block) {
		if(block != null){
			restore( player, block );
		}
	}

	
	/**
	 * 
	 */
	@Override
	public void playerRightClick(Player player, Block block) {
		if(block != null){
			restore( player, block );
		}
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 */
	protected void restore( Player player, Block block ){
		
		plugin.eventTimer.recordTimedEvent("rollback wand used");

		// Build params
		QueryParameters params = new QueryParameters();
		params.setWorld( player.getWorld().getName() );
		params.setSpecificBlockLocation( block.getLocation());
		params.setLimit(1);
		
		// Append actions that can be rolled back
		ArrayList<ActionType> types = ActionType.getCanRollbackActionTypes();
		for(ActionType type : types){
			params.addActionType(type);
		}
		
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( player, params );
		if(!results.getActionResults().isEmpty()){
			Restore rb = new Restore( plugin, player, PrismProcessType.RESTORE, results.getActionResults(), params );
			rb.apply();
		} else {
			String space_name = (block.getType().equals(Material.AIR) ? "space" : block.getType().toString().toLowerCase() + " block");
			player.sendMessage( plugin.playerError( "Nothing to restore for this " + space_name + " found." ) );
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
