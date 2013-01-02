package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.BlockAction;

import org.bukkit.Material;
import org.bukkit.block.BlockState;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.world.StructureGrowEvent;

public class PrismWorldEvents implements Listener {

	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public PrismWorldEvents( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onStructureGrow(StructureGrowEvent event) {
		
		String type = "tree-grow";
		
		//Sort out structure type
		if (event.getSpecies().name().toLowerCase().contains("mushroom")) type = "mushroom-grow";

		//Loop through blocks
		for (BlockState block : event.getBlocks()) {

			//Don't log the bottom block
			if (block.getType() == Material.MYCEL || block.getType() == Material.DIRT || block.getType() == Material.GRASS) continue;
	
			//If a player did it
			if (event.getPlayer() != null) {
				plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType(type), block.getBlock(), event.getPlayer().getName()) );
			}
			//If the environment did it
			else {
				plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType(type), block.getBlock(), "Environment") );
			}
		}
	}
}