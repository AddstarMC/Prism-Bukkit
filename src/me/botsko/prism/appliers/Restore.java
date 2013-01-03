package me.botsko.prism.appliers;

import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.BlockAction;

public class Restore extends Applier {

	
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
	 */
	private QueryParameters parameters;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Restore( Prism plugin, Player player, List<Action> results, QueryParameters parameters ) {
		this.plugin = plugin;
		this.player = player;
		this.results = results;
		this.parameters = parameters;
	}
	
	
	/**
	 * 
	 */
	public void restore(){
		
		if(!results.isEmpty()){
			
			// Inform nearby players
			plugin.notifyNearby(player, parameters.getRadius(), player.getDisplayName() + " is re-applying block changes nearby. Just so you know.");
			
			int restored_count = 0, skipped_block_count = 0;
			
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
					if(a.getType().doesCreateBlock()){
						// @todo ensure we're not removing a new block that's been placed by someone else
						if(block.getType().equals(Material.AIR)){
							
							if(!mayEverPlace(Material.getMaterial(b.getBlock_id()))){
								skipped_block_count++;
								continue;
							}
							
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
			
			// Build the results message
			String msg = restored_count + " events restored.";
			if(skipped_block_count > 0){
				msg += " " + skipped_block_count + " skipped.";
			}
			if(restored_count > 0){
				msg += ChatColor.GRAY + " It's like it was always there.";
			}
			player.sendMessage( plugin.playerHeaderMsg( msg ) );
			
		} else {
			player.sendMessage( plugin.playerError( "Nothing found to restore. Try using /prism l (args) first." ) );
		}
	}
}