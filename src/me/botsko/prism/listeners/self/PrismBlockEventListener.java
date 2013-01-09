package me.botsko.prism.listeners.self;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.PrismRollbackAction;
import me.botsko.prism.events.PrismBlocksRollbackEvent;
import me.botsko.prism.events.containers.BlockStateChange;

import org.bukkit.block.BlockState;
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
	public void onPrismBlocksRollbackEvent(PrismBlocksRollbackEvent event){
		plugin.debug("---- PrismBlocksRollbackEvent: " + event.getOnBehalfOf());
		ArrayList<BlockStateChange> blockStateChanges = event.getBlockStateChanges();
		if(!blockStateChanges.isEmpty()){
			for(BlockStateChange stateChange : blockStateChanges){
				
				BlockState orig = stateChange.getOriginalBlock();
				BlockState newBlock = stateChange.getNewBlock();
				
				PrismRollbackAction action = new PrismRollbackAction(ActionType.PRISM_ROLLBACK, orig.getTypeId(), orig.getRawData(), newBlock.getTypeId(), newBlock.getRawData(), event.getOnBehalfOf());
				
				action.setWorld_name(orig.getWorld().getName());
				action.setX(orig.getX());
				action.setY(orig.getY());
				action.setZ(orig.getZ());
				
				
				plugin.actionsRecorder.addToQueue( action );
				plugin.debug("Replaced block due to rollback/restore. Changed " + stateChange.getOriginalBlock().getTypeId() + " to " + stateChange.getNewBlock().getTypeId());
			}
		}
	}
}