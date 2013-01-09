package me.botsko.prism.events;

import org.bukkit.block.BlockState;
import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
 
public class PrismBlockReplaceEvent extends Event {
	
	/**
	 * Required by bukkit for proper event handling.
	 */
    private static final HandlerList handlers = new HandlerList();
    
    /**
     * 
     */
    private BlockState originalBlock;
    
    /**
     * 
     */
    private BlockState newBlock;
    
    /**
     * 
     */
    private String onBehalfOf;
 
    
    /**
     * 
     * @param example
     */
    public PrismBlockReplaceEvent( BlockState originalBlock, BlockState newBlock, String onBehalfOf ) {
        this.originalBlock = originalBlock;
        this.newBlock = newBlock;
        this.onBehalfOf = onBehalfOf;
    }
 
    
    /**
	 * @return the originalBlock
	 */
	public BlockState getOriginalBlock() {
		return originalBlock;
	}


	/**
	 * @return the newBlock
	 */
	public BlockState getNewBlock() {
		return newBlock;
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
