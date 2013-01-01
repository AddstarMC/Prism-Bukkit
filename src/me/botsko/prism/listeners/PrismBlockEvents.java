package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.BlockAction;

import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.block.BlockBurnEvent;
import org.bukkit.event.block.BlockFadeEvent;
import org.bukkit.event.block.BlockFormEvent;
import org.bukkit.event.block.BlockPlaceEvent;

public class PrismBlockEvents implements Listener {
	
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public PrismBlockEvents( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
    public void onBlockBreak(final BlockBreakEvent event){
		
		// Someone cancelled this before we did 
		if ( event.isCancelled() ) {
			return;
		}
		
		Player player = event.getPlayer();
		
//		// skip if we don't track creative mode
//		if(player.getGameMode() == GameMode.CREATIVE){
//			return;
//		}

		plugin.actionsRecorder.addToQueue( new BlockAction("block-break", event.getBlock(), player.getName()) );
		
	}
	
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
    public void onBlockPlace(final BlockPlaceEvent event){
		
		// Someone cancelled this before we did 
		if ( event.isCancelled() ) {
			return;
		}
		
		Player player = event.getPlayer();
		
//		// skip if we don't track creative mode
//		if(player.getGameMode() == GameMode.CREATIVE){
//			return;
//		}
		
		plugin.actionsRecorder.addToQueue( new BlockAction("block-place", event.getBlock(), player.getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
	public void onBlockForm(BlockFormEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction("block-form", event.getBlock(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
	public void onBlockFade(BlockFadeEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction("block-fade", event.getBlock(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
	public void onBlockBurn(BlockBurnEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction("block-burn", event.getBlock(), "Environment") );
	}
	
	//onSignChange
	//onLeavesDecay
	//onBlockFromTo
}