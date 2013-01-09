package me.botsko.prism.listeners.self;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.events.PrismBlocksRollbackEvent;
import me.botsko.prism.events.containers.BlockStateChange;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

public class PrismBlockEventListener implements Listener {
	
	/**
	 * 
	 */
	private Prism plugin;

	
	/**
	 * 
	 * @param plugin
	 */
	public PrismBlockEventListener( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler
	public void onPrismBlockReplaceEvent(PrismBlocksRollbackEvent event){
		plugin.debug("---- PrismBlockReplaceEvent: " + event.getOnBehalfOf());
		ArrayList<BlockStateChange> blockStateChanges = event.getBlockStateChanges();
		if(!blockStateChanges.isEmpty()){
			for(BlockStateChange stateChange : blockStateChanges){
				plugin.debug("Replaced block due to rollback/restore. Changed " + stateChange.getOriginalBlock().getTypeId() + " to " + stateChange.getNewBlock().getTypeId());
			}
		}
	}
}