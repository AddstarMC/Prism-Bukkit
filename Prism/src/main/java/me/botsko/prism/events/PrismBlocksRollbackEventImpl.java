package me.botsko.prism.events;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.api.events.PrismRollBackEvent;
import me.botsko.prism.api.BlockStateChange;
import me.botsko.prism.appliers.ApplierResult;
import org.bukkit.entity.Player;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class PrismBlocksRollbackEventImpl extends PrismRollBackEvent {

    private static final HandlerList handlers = new HandlerList();
    private final List<BlockStateChange> blockStateChanges;
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
    public PrismBlocksRollbackEventImpl(List<BlockStateChange> blockStateChanges, Player onBehalfOf,
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
    public List<BlockStateChange> getBlockStateChanges() {
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
