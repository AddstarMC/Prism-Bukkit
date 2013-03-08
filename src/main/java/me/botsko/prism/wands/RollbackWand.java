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
import me.botsko.prism.appliers.PrismApplierCallback;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.appliers.Rollback;

public class RollbackWand extends WandBase implements Wand{

	/**
	 * 
	 */
	private Prism plugin;

	/**
	 * 
	 */
	protected boolean item_given = false;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public RollbackWand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 */
	public void playerLeftClick(Player player, Block block) {
		if(block != null){
			rollback( player, block );
		}
	}

	
	/**
	 * 
	 */
	public void playerRightClick(Player player, Block block) {
		if(block != null){
			rollback( player, block );
		}
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 */
	protected void rollback( Player player, Block block ){
		
		plugin.eventTimer.recordTimedEvent("rollback wand used");

		// Build params
		QueryParameters params = new QueryParameters();
		params.setWorld( player.getWorld().getName() );
		params.setSpecificBlockLocation( block.getLocation());
		params.setLimit(1);
		
		// Append actions that can be rolled back
		ArrayList<String> types = Prism.getActionRegistry().listActionsThatAllowRollback();
		for(String type : types){
			params.addActionType(type);
		}
		
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( params, player );
		if(!results.getActionResults().isEmpty()){
			Rollback rb = new Rollback( plugin, player, PrismProcessType.ROLLBACK, results.getActionResults(), params, new PrismApplierCallback() );
			rb.apply();
		} else {
			String space_name = (block.getType().equals(Material.AIR) ? "space" : block.getType().toString().toLowerCase() + " block");
			player.sendMessage( Prism.messenger.playerError( "Nothing to rollback for this " + space_name + " found." ) );
		}
	}
	
	
	/**
	 * 
	 */
	public void playerRightClick(Player player, Entity entity) {
		return;
	}


	/**
	 * 
	 */
	public void setItemWasGiven(boolean given) {
		this.item_given = given;
	}


	/**
	 * 
	 */
	public boolean itemWasGiven() {
		return item_given;
	}
}