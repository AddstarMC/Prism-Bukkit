package me.botsko.prism.rollback;

import java.util.List;

import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.block.Block;

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
	private List<Action> results;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Rollback( Prism plugin, List<Action> results ) {
		this.plugin = plugin;
		this.results = results;
	}
	
	
	/**
	 * 
	 */
	public void rollback(){
		
		if(!results.isEmpty()){
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
					
					block.setTypeId( b.getBlock_id() );
					block.setData( b.getBlock_subid() );
					
				}
				
				
				/**
				 * Rollback entity kills
				 */
				if( a instanceof EntityKillAction ){
					
					EntityKillAction b = (EntityKillAction) a;
					world.spawnEntity(loc, b.getEntityTypeFromData());
					
					plugin.debug("Rolling back entity " + b.getEntityTypeFromData().getName());
					
				}
			}
		}
	}
}