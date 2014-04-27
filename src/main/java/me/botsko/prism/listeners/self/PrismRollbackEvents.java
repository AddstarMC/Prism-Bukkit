package me.botsko.prism.listeners.self;

import me.botsko.prism.events.PrismBlocksRollbackEvent;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

public class PrismRollbackEvents implements Listener {

    /**
     * 
     * @param event
     */
    @EventHandler
    public void onPrismBlocksRollbackEvent(final PrismBlocksRollbackEvent event) {

        // // Get all block changes for this event
        // ArrayList<BlockStateChange> blockStateChanges =
        // event.getBlockStateChanges();
        // if(!blockStateChanges.isEmpty()){
        //
        // // Create an entry for the rollback as a whole
        // Handler primaryAction = ActionFactory.createBlock("prism-process",
        // PrismProcessType.ROLLBACK, event.onBehalfOf(),
        // event.getCommandParams() );
        // int id = ActionQueue.insertActionIntoDatabase( primaryAction );
        // if(id == 0){
        // return;
        // }
        // for(BlockStateChange stateChange : blockStateChanges){
        //
        // BlockState orig = stateChange.getOriginalBlock();
        // BlockState newBlock = stateChange.getNewBlock();
        //
        // // Build the action
        // PrismRollbackAction action = new
        // PrismRollbackAction("prism-rollback", orig.getTypeId(),
        // orig.getRawData(), newBlock.getTypeId(), newBlock.getRawData(),
        // event.onBehalfOf().getName(), id);
        // action.setWorldName(orig.getWorld().getName());
        // action.setX(orig.getX());
        // action.setY(orig.getY());
        // action.setZ(orig.getZ());
        //
        // ActionQueue.addToQueue( action );
        // }
        // ActionQueue.save();
        // }
    }
}