package me.botsko.prism.listeners;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.block.BlockBurnEvent;
import org.bukkit.event.block.BlockFadeEvent;
import org.bukkit.event.block.BlockFormEvent;
import org.bukkit.event.block.BlockIgniteEvent;
import org.bukkit.event.block.BlockPhysicsEvent;
import org.bukkit.event.block.BlockPlaceEvent;
import org.bukkit.event.block.LeavesDecayEvent;
import org.bukkit.event.block.SignChangeEvent;
import org.bukkit.event.player.PlayerBucketEmptyEvent;

public class PrismBlockEvents implements Listener {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * We store a basic index of block we anticipate will fall, so
	 * that when they do fall we can attribute them to the player who
	 * broke the original block.
	 * 
	 * Once the block fall is registered, it's removed from here, so
	 * data should not remain here long.
	 */
	private ConcurrentHashMap<String,String> preplannedBlockFalls = new ConcurrentHashMap<String,String>();
	
	
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
		plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType("block-break"), event.getBlock(), player.getName()) );
		
		// Find a list of all blocks above this block that we know
		// will fall. 
		ArrayList<Block> falling_blocks = BlockUtils.findFallingBlocksAboveBlock(event.getBlock());
		if(falling_blocks.size() > 0){
			for(Block b : falling_blocks){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				plugin.debug("Anticipating falling block at " + coord_key + " for " + player.getName());
				preplannedBlockFalls.put(coord_key, player.getName());
			}
		}
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
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockPhysics(BlockPhysicsEvent event) {
		
		// Record that a block fell, associated with the player who broke the base block.
		Block b = event.getBlock();
		if(BlockUtils.isFallingBlock(b)){
			// Only record a block-fall if there's air below.
			if(b.getRelative(BlockFace.DOWN).getType().equals(Material.AIR)){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				if(preplannedBlockFalls.containsKey(coord_key)){
					String player = preplannedBlockFalls.get(coord_key);
					plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType("block-fall"), b, player) );
					preplannedBlockFalls.remove(coord_key);
				}
			}
		}
		// Get sign/torch/itemframe that was attached
//		if (b.getState().getData() instanceof Attachable) {
//			Attachable a = (Attachable)	b.getState().getData();
//			Block attachedBlock = b.getRelative(a.getAttachedFace());
//			if (attachedBlock.getTypeId() == 0) {
//			// attached to air? looks like the sign (or other attachable) has become detached
//
//			}
//		}
		
//		plugin.actionsRecorder.addToQueue( new BlockAction(plugin.getActionType("block-burn"), event.getBlock(), "Environment") );
	}
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onSignChange(SignChangeEvent event) {
		plugin.actionsRecorder.addToQueue( new SignAction(plugin.getActionType("sign-change"), event.getBlock(), event.getLines(), event.getPlayer().getName()) );
	}


	/**
	 * 
	 * @param event
	 */
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