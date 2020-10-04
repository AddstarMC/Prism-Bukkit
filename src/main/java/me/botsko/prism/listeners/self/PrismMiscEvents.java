package me.botsko.prism.listeners.self;

import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.actionlibs.RecordingTask;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksDrainEvent;
import me.botsko.prism.events.PrismBlocksExtinguishEvent;
import org.bukkit.block.BlockState;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import java.util.ArrayList;

public class PrismMiscEvents implements Listener {

    /**
     * PrismBlocksDrainEvent.
     *
     * @param event PrismBlocksDrainEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPrismBlocksDrainEvent(final PrismBlocksDrainEvent event) {

        // Get all block changes for this event
        final ArrayList<BlockStateChange> blockStateChanges = event.getBlockStateChanges();
        if (!blockStateChanges.isEmpty()) {

            // Create an entry for the rollback as a whole
            final Handler primaryAction = ActionFactory.createPrismProcess("prism-process", PrismProcessType.DRAIN,
                    event.onBehalfOf(), "" + event.getRadius());
            final long id = RecordingTask.insertActionIntoDatabase(primaryAction);
            if (id == 0) {
                return;
            }
            for (final BlockStateChange stateChange : blockStateChanges) {

                final BlockState orig = stateChange.getOriginalBlock();
                final BlockState newBlock = stateChange.getNewBlock();

                // Build the action
                RecordingQueue.addToQueue(
                        ActionFactory.createPrismRollback("prism-drain", orig, newBlock, event.onBehalfOf(), id));

            }
            // ActionQueue.save();
        }
    }

    /**
     * PrismBlocksExtinguishEvent.
     * @param event PrismBlocksExtinguishEvent.
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPrismBlocksExtinguishEvent(final PrismBlocksExtinguishEvent event) {

        // Get all block changes for this event
        final ArrayList<BlockStateChange> blockStateChanges = event.getBlockStateChanges();
        if (!blockStateChanges.isEmpty()) {

            // Create an entry for the rollback as a whole
            final Handler primaryAction = ActionFactory.createPrismProcess("prism-process", PrismProcessType.EXTINGUISH,
                    event.onBehalfOf(), "" + event.getRadius());
            final long id = RecordingTask.insertActionIntoDatabase(primaryAction);
            if (id == 0) {
                return;
            }
            for (final BlockStateChange stateChange : blockStateChanges) {

                final BlockState orig = stateChange.getOriginalBlock();
                final BlockState newBlock = stateChange.getNewBlock();

                // Build the action
                RecordingQueue.addToQueue(
                        ActionFactory.createPrismRollback("prism-extinguish", orig, newBlock, event.onBehalfOf(), id));

            }
            // ActionQueue.save();
        }
    }
}