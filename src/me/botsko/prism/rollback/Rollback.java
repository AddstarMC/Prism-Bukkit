package me.botsko.prism.rollback;

import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.EntityKillAction;

public class Rollback {

	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private Player player;
	
	/**
	 * 
	 */
	private List<Action> results;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Rollback( Prism plugin, Player player, List<Action> results ) {
		this.plugin = plugin;
		this.player = player;
		this.results = results;
	}
	
	
	/**
	 * 
	 */
	public void rollback(){
		
		if(!results.isEmpty()){
			
			int rolled_back_count = 0;
			
			for(Action a : results){
				
				World world = plugin.getServer().getWorld(a.getWorld_name());
				
				//Get some data from the entry
				Location loc = new Location(world, a.getX(), a.getY(), a.getZ());
				
				/**
				 * Rollback block changes
				 */
				if( a instanceof BlockAction ){
					
//					plugin.debug("Rolling back blocks");
					
					BlockAction b = (BlockAction) a;
					
					Block block = world.getBlockAt(loc);
//					BlockState state = block.getState();
					
					
					// If the block was placed, we need to remove it
					// @todo it may not always be air that was replaced. we should log that
					if(a.getAction_type().equals("block-place") || a.getAction_type().equals("block-form")){
						block.setType(Material.AIR);
					} else {
						// Otherwise, add it back
						block.setTypeId( b.getBlock_id() );
						block.setData( b.getBlock_subid() );
					}
					rolled_back_count++;
				}
				
				
				/**
				 * Rollback entity kills
				 */
				if( a instanceof EntityKillAction ){
					
					EntityKillAction b = (EntityKillAction) a;
					world.spawnEntity(loc, b.getEntityTypeFromData());
					
					plugin.debug("Rolling back entity " + b.getEntityTypeFromData().getName());
					
					rolled_back_count++;
					
				}
			}
			
			player.sendMessage( plugin.playerHeaderMsg( rolled_back_count + " reversals." + ChatColor.GRAY + " It's like it never happened." ) );
			
		} else {
			player.sendMessage( plugin.playerError( "Nothing found to rollback. Try using /prism l (args) first." ) );
		}
	}
}