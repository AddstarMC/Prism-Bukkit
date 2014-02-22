package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.block.BlockState;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.world.StructureGrowEvent;
import org.bukkit.event.world.WorldLoadEvent;

public class PrismWorldEvents implements Listener {
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onStructureGrow(final StructureGrowEvent event) {
		String type = "tree-grow";
		if (event.getSpecies().name().toLowerCase().contains("mushroom")) type = "mushroom-grow";
		if( !Prism.getIgnore().event(type,event.getWorld()) ) return;
		for (BlockState block : event.getBlocks()) {
			if(BlockUtils.isGrowableStructure( block.getType() )){
				String player = "Environment";
				if (event.getPlayer() != null){
					player = event.getPlayer().getName();
				}
				RecordingQueue.addToQueue( ActionFactory.create(type, block, player) );
			}
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onWorldLoad(final WorldLoadEvent event){
		
		String worldName = event.getWorld().getName();
		
		if( !Prism.prismWorlds.containsKey( worldName ) ){
			Prism.addWorldName( worldName );
		}
	}
}