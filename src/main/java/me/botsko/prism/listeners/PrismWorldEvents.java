package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.GrowAction;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.block.BlockState;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.world.StructureGrowEvent;

public class PrismWorldEvents implements Listener {
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onStructureGrow(final StructureGrowEvent event) {
		String type = "tree-grow";
		if (event.getSpecies().name().toLowerCase().contains("mushroom")) type = "mushroom-grow";
		for (BlockState block : event.getBlocks()) {
			if(BlockUtils.isGrowableStructure( block.getType() )){
				String player = "Environment";
				if (event.getPlayer() != null){
					player = event.getPlayer().getName();
				}
				Prism.actionsRecorder.addToQueue( new GrowAction(type, block, player) );
			}
		}
	}
}