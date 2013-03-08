package me.botsko.prism.listeners.self;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actions.Handler;
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
	 * @param event
	 */
	@EventHandler
	public void onPrismBlocksDrainEvent(final PrismBlocksDrainEvent event){

		// Get all block changes for this event
		ArrayList<BlockStateChange> blockStateChanges = event.getBlockStateChanges();
		if(!blockStateChanges.isEmpty()){
			
			// Create an entry for the rollback as a whole
			Handler primaryAction = ActionFactory.create("prism-process", PrismProcessType.DRAIN, event.onBehalfOf(), ""+event.getRadius() );
			int id = Prism.actionsRecorder.insertActionIntoDatabase( primaryAction );
			if(id == 0){
				return;
			}
			for(BlockStateChange stateChange : blockStateChanges){
				
				BlockState orig = stateChange.getOriginalBlock();
				BlockState newBlock = stateChange.getNewBlock();

				// Build the action
				Prism.actionsRecorder.addToQueue( ActionFactory.create("prism-drain", orig, newBlock, event.onBehalfOf().getName(), id) );
				
			}
			Prism.actionsRecorder.save();
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
			Handler primaryAction = ActionFactory.create("prism-process", PrismProcessType.EXTINGUISH, event.onBehalfOf(), ""+event.getRadius() );
			int id = Prism.actionsRecorder.insertActionIntoDatabase( primaryAction );
			if(id == 0){
				return;
			}
			for(BlockStateChange stateChange : blockStateChanges){
				
				BlockState orig = stateChange.getOriginalBlock();
				BlockState newBlock = stateChange.getNewBlock();

				// Build the action
				Prism.actionsRecorder.addToQueue( ActionFactory.create("prism-extinguish", orig, newBlock, event.onBehalfOf().getName(), id) );
				
			}
			Prism.actionsRecorder.save();
		}
	}
}