package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.BlockAction;

import org.bukkit.Material;
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
import org.bukkit.event.player.PlayerBucketEmptyEvent;

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
		// Pull the proper action type class
		plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType("block-break"), event.getBlock(), player.getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockPlace(final BlockPlaceEvent event){
		Player player = event.getPlayer();
		plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType("block-place"), event.getBlock(), player.getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockForm(BlockFormEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType("block-form"), event.getBlock(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockFade(BlockFadeEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType("block-fade"), event.getBlock(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onLeavesDecay(LeavesDecayEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType("leaf-decay"), event.getBlock(), "Environment") );
	}
	

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockBurn(BlockBurnEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType("block-burn"), event.getBlock(), "Environment") );
	}
	
	//onSignChange
	//onBlockFromTo

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onSetFire(final BlockIgniteEvent event){
		
		String cause = null;
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
//				cause = "fire-spread";
				break;
			default:
//				cause = "block-ignite";
		}
		if(cause != null){
			Player player = event.getPlayer();
			plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType(cause), event.getBlock(), (player == null ? "Environment" : player.getName())) );
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerBucketEmpty(final PlayerBucketEmptyEvent event){
		Player player = event.getPlayer();
		String cause = (event.getBucket() == Material.LAVA_BUCKET ? "lava-bucket" : "water-bucket");
		plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType(cause), event.getBlockClicked().getRelative(event.getBlockFace()), player.getName()) );
	}
}