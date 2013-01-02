package me.botsko.prism.appliers;

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

public class Restore {

	
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
	public Restore( Prism plugin, Player player, List<Action> results ) {
		this.plugin = plugin;
		this.player = player;
		this.results = results;
	}
	
	
	/**
	 * 
	 */
	public void restore(){
		
		if(!results.isEmpty()){
			
			int restored_count = 0;
			
			for(Action a : results){
				
				World world = plugin.getServer().getWorld(a.getWorld_name());
				
				//Get some data from the entry
				Location loc = new Location(world, a.getX(), a.getY(), a.getZ());
				
				/**
				 * Restore block changes
				 */
				if( a instanceof BlockAction ){
					
					BlockAction b = (BlockAction) a;
					
					Block block = world.getBlockAt(loc);
//					BlockState state = block.getState();
					
					
					// If the block was placed, we must replace it
					if(a.getAction_type().equals("block-place") || a.getAction_type().equals("block-form")){
						// @todo ensure we're not removing a new block that's been placed by someone else
						if(block.getType().equals(Material.AIR)){
							block.setTypeId( b.getBlock_id() );
							block.setData( b.getBlock_subid() );
							restored_count++;
						}
					} else {
						
						/**
						 * Again remove the block that was removed
						 */
						if(!block.getType().equals(Material.AIR)){
							block.setType( Material.AIR);
							restored_count++;
						}
					}
				}
			}
			
			player.sendMessage( plugin.playerHeaderMsg( restored_count + " events restored." + ChatColor.GRAY + " It's like it was always there." ) );
			
		} else {
			player.sendMessage( plugin.playerError( "Nothing found to restore. Try using /prism l (args) first." ) );
		}
	}
}