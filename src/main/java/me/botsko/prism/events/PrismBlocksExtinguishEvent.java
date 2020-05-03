package me.botsko.prism.events;

import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;

public class PrismBlocksExtinguishEvent extends Event {

    private static final HandlerList handlers = new HandlerList();

    protected final int radius;

    private final ArrayList<BlockStateChange> blockStateChanges;

    private final Player onBehalfOf;

    /**
     * Constructor.
     *
     * @param blockStateChanges List
     * @param onBehalfOf        Player
     * @param radius            int
     */
    public PrismBlocksExtinguishEvent(ArrayList<BlockStateChange> blockStateChanges, Player onBehalfOf, int radius) {
        this.blockStateChanges = blockStateChanges;
        this.onBehalfOf = onBehalfOf;
        this.radius = radius;
    }


    public ArrayList<BlockStateChange> getBlockStateChanges() {
        return blockStateChanges;
    }

    public Player onBehalfOf() {
        return onBehalfOf;
    }

    public int getRadius() {
        return radius;
    }

    /**
     * Required by bukkit for proper event handling.
     */
    @NotNull
    @Override
    public HandlerList getHandlers() {
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
