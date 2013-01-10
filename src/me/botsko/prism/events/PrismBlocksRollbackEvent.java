package me.botsko.prism.events;


import java.util.ArrayList;


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
    private ArrayList<BlockStateChange> blockStateChanges;
    
    /**
     * 
     */
    private String onBehalfOf;
 
    
    /**
     * 
     * @param example
     */
    public PrismBlocksRollbackEvent( ArrayList<BlockStateChange> blockStateChanges, String onBehalfOf ) {
        this.blockStateChanges = blockStateChanges;
        this.onBehalfOf = onBehalfOf;
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
	public String getOnBehalfOf() {
		return onBehalfOf;
	}


	/**
     * Required by bukkit for proper event handling.
     */
    public HandlerList getHandlers() {
        return handlers;
    }
 
    /**
     * Required by bukkit for proper event handling.
     * @return
     */
    public static HandlerList getHandlerList() {
        return handlers;
    }
}
