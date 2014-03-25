package me.botsko.prism.events;

import java.util.ArrayList;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ApplierResult;

import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

public class PrismBlocksRollbackEvent extends Event {

    /**
     * Required by bukkit for proper event handling.
     */
    private static final HandlerList handlers = new HandlerList();

    /**
     * 
     */
    private final ArrayList<BlockStateChange> blockStateChanges;

    /**
     * 
     */
    private final Player onBehalfOf;

    /**
     * 
     */
    private final QueryParameters parameters;

    /**
     * 
     */
    private final ApplierResult result;

    /**
     * 
     * @param blockStateChanges
     * @param onBehalfOf
     * @param parameters
     * @param result
     */
    public PrismBlocksRollbackEvent(ArrayList<BlockStateChange> blockStateChanges, Player onBehalfOf,
            QueryParameters parameters, ApplierResult result) {
        this.blockStateChanges = blockStateChanges;
        this.onBehalfOf = onBehalfOf;
        this.parameters = parameters;
        this.result = result;
    }

    /**
     * @return the originalBlock
     */
    public ArrayList<BlockStateChange> getBlockStateChanges() {
        return blockStateChanges;
    }

    /**
     * @return the onBehalfOf
     */
    public Player onBehalfOf() {
        return onBehalfOf;
    }

    /**
     * 
     * @return
     */
    public QueryParameters getQueryParameters() {
        return parameters;
    }

    /**
     * 
     * @return
     */
    public ApplierResult getResult() {
        return result;
    }

    /**
     * Required by bukkit for proper event handling.
     */
    @Override
    public HandlerList getHandlers() {
        return handlers;
    }

    /**
     * Required by bukkit for proper event handling.
     * 
     * @return
     */
    public static HandlerList getHandlerList() {
        return handlers;
    }
}
