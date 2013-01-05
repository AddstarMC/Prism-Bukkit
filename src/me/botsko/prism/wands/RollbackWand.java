package me.botsko.prism.wands;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.Rollback;

public class RollbackWand implements Wand {

	/**
	 * 
	 */
	private Prism plugin;
	
	
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
	@Override
	public void playerLeftClick(Player player, Block block) {
		if(block != null){
			rollback( player, block );
		}
	}

	
	/**
	 * 
	 */
	@Override
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
		
		plugin.debug("Running rollback wand query for " + block.getX() + " " + block.getY() + " " + block.getZ());

		// Build params
		QueryParameters params = new QueryParameters();
		params.setWorld( player.getWorld().getName() );
		params.setLoc( block.getLocation());
		
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( player, params );
		if(!results.getActionResults().isEmpty()){
			Rollback rb = new Rollback( plugin, player, results.getActionResults(), params );
			rb.apply();
		} else {
			String space_name = (block.getType().equals(Material.AIR) ? "space" : block.getType().toString().toLowerCase() + " block");
			player.sendMessage( plugin.playerError( "Nothing to rollback for this " + space_name + " found." ) );
		}
	}
}
