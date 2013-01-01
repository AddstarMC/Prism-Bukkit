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
import org.bukkit.event.block.BlockIgniteEvent;
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
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockBreak(final BlockBreakEvent event){
		Player player = event.getPlayer();
		plugin.actionsRecorder.addToQueue( new BlockAction("block-break", event.getBlock(), player.getName()) );
	}
	
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockPlace(final BlockPlaceEvent event){
		Player player = event.getPlayer();
		plugin.actionsRecorder.addToQueue( new BlockAction("block-place", event.getBlock(), player.getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockForm(BlockFormEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction("block-form", event.getBlock(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockFade(BlockFadeEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction("block-fade", event.getBlock(), "Environment") );
	}
	

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockBurn(BlockBurnEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction("block-burn", event.getBlock(), "Environment") );
	}
	
	//onSignChange
	//onLeavesDecay
	//onBlockFromTo

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onSetFire(final BlockIgniteEvent event){
		
		String cause;
		switch (event.getCause()){
			case FIREBALL:
				cause = "fireball";
			case FLINT_AND_STEEL:
				cause = "flint-steel";
			case LAVA:
				cause = "lava-ignite";
			case LIGHTNING:
				cause = "lightning";
			case SPREAD:
				cause = "fire-spread";
			default:
				cause = "block-ignite";
		}
		
		Player player = event.getPlayer();
		plugin.actionsRecorder.addToQueue( new BlockAction(cause, event.getBlock(), (player == null ? "Environment" : player.getName())) );
		
	}
}