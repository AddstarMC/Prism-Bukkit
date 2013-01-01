package me.botsko.prism.rollback;

import java.util.List;

import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.block.Block;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.BlockAction;

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
				
				
				if(a instanceof BlockAction){
					
					BlockAction b = (BlockAction) a;
					
					Block block = world.getBlockAt(loc);
//					BlockState state = block.getState();
					
					block.setTypeId( b.getBlock_id() );
					block.setData( b.getBlock_subid() );
					
				}
			}
		}
	}
}