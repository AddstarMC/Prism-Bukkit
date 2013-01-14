package me.botsko.prism.listeners.self;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.PrismProcessAction;
import me.botsko.prism.actions.PrismRollbackAction;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksDrainEvent;
import me.botsko.prism.events.PrismBlocksExtinguishEvent;

import org.bukkit.block.BlockState;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

public class PrismMiscEvents implements Listener {
	
	/**
	 * 
	 */
	private Prism plugin;

	
	/**
	 * 
	 * @param plugin
	 */
	public PrismMiscEvents( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler
	public void onPrismBlocksDrainEvent(final PrismBlocksDrainEvent event){

		// Get all block changes for this event
		ArrayList<BlockStateChange> blockStateChanges = event.getBlockStateChanges();
		if(!blockStateChanges.isEmpty()){
			
			// Create an entry for the rollback as a whole
			PrismProcessAction primaryAction = new PrismProcessAction(ActionType.PRISM_PROCESS, PrismProcessType.DRAIN, event.onBehalfOf(), ""+event.getRadius() );
			int id = plugin.actionsRecorder.insertActionIntoDatabase( primaryAction );
			if(id == 0){
				return;
			}
			plugin.actionsRecorder.shouldImmediatelyProcessQueue(false);
			for(BlockStateChange stateChange : blockStateChanges){
				
				BlockState orig = stateChange.getOriginalBlock();
				BlockState newBlock = stateChange.getNewBlock();

				// Build the action
				PrismRollbackAction action = new PrismRollbackAction(ActionType.PRISM_DRAIN, orig.getTypeId(), orig.getRawData(), newBlock.getTypeId(), newBlock.getRawData(), event.onBehalfOf().getName(), id);
				action.setWorld_name(orig.getWorld().getName());
				action.setX(orig.getX());
				action.setY(orig.getY());
				action.setZ(orig.getZ());

				plugin.actionsRecorder.addToQueue( action );
			}
			plugin.actionsRecorder.saveQueue();
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler
	public void onPrismBlocksExtinguishEvent(final PrismBlocksExtinguishEvent event){

		// Get all block changes for this event
		ArrayList<BlockStateChange> blockStateChanges = event.getBlockStateChanges();
		if(!blockStateChanges.isEmpty()){
			
			// Create an entry for the rollback as a whole
			PrismProcessAction primaryAction = new PrismProcessAction(ActionType.PRISM_PROCESS, PrismProcessType.EXTINGUISH, event.onBehalfOf(), ""+event.getRadius() );
			int id = plugin.actionsRecorder.insertActionIntoDatabase( primaryAction );
			if(id == 0){
				return;
			}
			plugin.actionsRecorder.shouldImmediatelyProcessQueue(false);
			for(BlockStateChange stateChange : blockStateChanges){
				
				BlockState orig = stateChange.getOriginalBlock();
				BlockState newBlock = stateChange.getNewBlock();

				// Build the action
				PrismRollbackAction action = new PrismRollbackAction(ActionType.PRISM_DRAIN, orig.getTypeId(), orig.getRawData(), newBlock.getTypeId(), newBlock.getRawData(), event.onBehalfOf().getName(), id);
				action.setWorld_name(orig.getWorld().getName());
				action.setX(orig.getX());
				action.setY(orig.getY());
				action.setZ(orig.getZ());

				plugin.actionsRecorder.addToQueue( action );
			}
			plugin.actionsRecorder.saveQueue();
		}
	}
}