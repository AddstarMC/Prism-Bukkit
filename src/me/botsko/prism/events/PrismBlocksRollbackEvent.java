package me.botsko.prism.events;


import java.util.ArrayList;


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
    private ArrayList<BlockStateChange> blockStateChanges;
    
    /**
     * 
     */
    private Player onBehalfOf;
    
    /**
     * 
     */
    private String commandParameters;
 
    
    /**
     * 
     * @param example
     */
    public PrismBlocksRollbackEvent( ArrayList<BlockStateChange> blockStateChanges, Player onBehalfOf, String commandParameters ) {
        this.blockStateChanges = blockStateChanges;
        this.onBehalfOf = onBehalfOf;
        this.commandParameters = commandParameters;
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
	public String getCommandParams(){
		return commandParameters;
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
