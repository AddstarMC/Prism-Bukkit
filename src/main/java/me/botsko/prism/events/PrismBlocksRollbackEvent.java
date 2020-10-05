package me.botsko.prism.events;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ApplierResult;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;

public class PrismBlocksRollbackEvent extends Event {

    private static final HandlerList handlers = new HandlerList();
    private final ArrayList<BlockStateChange> blockStateChanges;
    private final Player onBehalfOf;
    private final QueryParameters parameters;
    private final ApplierResult result;

    /**
     * Constructor.
     *
     * @param blockStateChanges List BlockStateChange
     * @param onBehalfOf        Player
     * @param parameters        QueryParameters
     * @param result            ApplierResult
     */
    public PrismBlocksRollbackEvent(ArrayList<BlockStateChange> blockStateChanges, Player onBehalfOf,
                                    QueryParameters parameters, ApplierResult result) {
        this.blockStateChanges = blockStateChanges;
        this.onBehalfOf = onBehalfOf;
        this.parameters = parameters;
        this.result = result;
    }

    /**
     * ArrayList.
     *
     * @return the originalBlock  List BlockStateChange
     */
    public ArrayList<BlockStateChange> getBlockStateChanges() {
        return blockStateChanges;
    }

    /**
     * Player.
     *
     * @return the onBehalfOf Player
     */
    public Player onBehalfOf() {
        return onBehalfOf;
    }

    /**
     * QueryParameters.
     *
     * @return QueryParameters
     */
    public QueryParameters getQueryParameters() {
        return parameters;
    }

    /**
     * ApplierResult.
     *
     * @return ApplierResult
     */
    public ApplierResult getResult() {
        return result;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    /**
     * Required by bukkit for proper event handling.
     */
    @SuppressWarnings("unused")
    public static HandlerList getHandlerList() {
        return handlers;

    }
}
