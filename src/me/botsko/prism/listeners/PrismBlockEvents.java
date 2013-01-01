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
import org.bukkit.event.block.LeavesDecayEvent;

public class PrismBlockEvents implements Listener {
	
	/**
	 * 
	 */
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
	public void onLeavesDecay(LeavesDecayEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction("leaf-decay", event.getBlock(), "Environment") );
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
	//onBlockFromTo

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onSetFire(final BlockIgniteEvent event){
		
		String cause;
		switch (event.getCause()){
			case FIREBALL:
				cause = "fireball";
				break;
			case FLINT_AND_STEEL:
				cause = "flint-steel";
				break;
			case LAVA:
				cause = "lava-ignite";
				break;
			case LIGHTNING:
				cause = "lightning";
				break;
			case SPREAD:
				cause = "fire-spread";
				break;
			default:
				cause = "block-ignite";
		}
		
		plugin.debug("Fire cause is " + event.getCause().name());
		
		Player player = event.getPlayer();
		plugin.actionsRecorder.addToQueue( new BlockAction(cause, event.getBlock(), (player == null ? "Environment" : player.getName())) );
		
	}
}