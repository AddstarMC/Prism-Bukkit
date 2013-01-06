package me.botsko.prism.listeners;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.block.Chest;
import org.bukkit.block.Dispenser;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.block.BlockBurnEvent;
import org.bukkit.event.block.BlockFadeEvent;
import org.bukkit.event.block.BlockFormEvent;
import org.bukkit.event.block.BlockFromToEvent;
import org.bukkit.event.block.BlockIgniteEvent;
import org.bukkit.event.block.BlockPhysicsEvent;
import org.bukkit.event.block.BlockPistonExtendEvent;
import org.bukkit.event.block.BlockPistonRetractEvent;
import org.bukkit.event.block.BlockPlaceEvent;
import org.bukkit.event.block.LeavesDecayEvent;
import org.bukkit.event.block.SignChangeEvent;
import org.bukkit.event.player.PlayerBucketEmptyEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.material.Attachable;
import org.bukkit.material.Bed;

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
	 * @param player_name
	 * @param block
	 */
	public void logItemRemoveFromDestroyedContainer( String player_name, Block block ){
		// If this is a container we need to trigger item removal for everything in it.
		// It's important we record this *after* the block break so the log shows what
		// really happened.
		if( block.getType().equals(Material.CHEST) ){
			Chest container = (Chest) block.getState();
			for( ItemStack i : container.getInventory().getContents()){
				if(i != null){
					plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_REMOVE, i, block.getLocation(), player_name) );
				}
			}
		}
		// @todo not working. might need to record items from each slot
//		if( block.getType().equals(Material.FURNACE) ){
//			Furnace container = (Furnace) block.getState();
//			for( ItemStack i : container.getInventory().getContents()){
//				if(i != null){
//					plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_REMOVE, i, block.getLocation(), player_name) );
//				}
//			}
//		}
		if( block.getType().equals(Material.DISPENSER) ){
			Dispenser container = (Dispenser) block.getState();
			for( ItemStack i : container.getInventory().getContents()){
				if(i != null){
					plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_REMOVE, i, block.getLocation(), player_name) );
				}
			}
		}
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 */
	protected void logBlockRelationshipsForBlock( Player player, Block block ){
		
		// Find a list of all blocks above this block that we know
		// will fall. 
		ArrayList<Block> falling_blocks = BlockUtils.findFallingBlocksAboveBlock(block);
		if(falling_blocks.size() > 0){
			for(Block b : falling_blocks){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				plugin.debug("Anticipating falling block at " + coord_key + " for " + player.getName());
				plugin.preplannedBlockFalls.put(coord_key, player.getName());
			}
		}
		
//		// We can't have attachments on attachments so we'll end here.
//		if(BlockUtils.isDetachableBlock(block)){
//			return;
//		}
		
		// Find a list of all blocks above this block that we know
		// will fall. 
		ArrayList<Block> detached_blocks = BlockUtils.findAttachedBlocks(block);
		if(detached_blocks.size() > 0){
			for(Block b : detached_blocks){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				plugin.debug("Anticipating block detaching at " + coord_key + " for " + player.getName());
				plugin.preplannedBlockFalls.put(coord_key, player.getName());
			}
		}
		
		
		// Find a list of all hanging entities on this block
		ArrayList<Entity> hanging = BlockUtils.findHangingEntities(block);
		if(hanging.size() > 0){
			for(Entity e : hanging){
				String coord_key = e.getLocation().getBlockX() + ":" + e.getLocation().getBlockY() + ":" + e.getLocation().getBlockZ();
				plugin.debug("Anticipating hanging item detaching at " + coord_key + " for " + player.getName());
				plugin.preplannedBlockFalls.put(coord_key, player.getName());
			}
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockBreak(final BlockBreakEvent event){
		Player player = event.getPlayer();
		Block block = event.getBlock();
		
		// Run ore find alerts
		plugin.oreMonitor.processAlertsFromBlock(player, block);
		
		/**
		 * Handle special double-length blocks
		 */
		if( block.getType().equals(Material.WOODEN_DOOR) || block.getType().equals(Material.IRON_DOOR_BLOCK) ){
			// If you've broken the top half of a door, we need to record the action for the bottom.
			// This is because a top half break doesn't record the orientation of the door while the bottom does,
			// and we have code in the rollback/restore to add the top half back in.
			if(block.getData() == 8){
				block = block.getRelative(BlockFace.DOWN);
			}
		}
		// If it's a bed, we always record the lower half and rely on appliers
		if( block.getType().equals(Material.BED_BLOCK) ){
			Bed b = (Bed)block.getState().getData();
			if(b.isHeadOfBed()){
	            block = block.getRelative(b.getFacing().getOppositeFace());
	        }
		}
		
		plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_BREAK, block, player.getName()) );
		
		// log items removed from chest
		logItemRemoveFromDestroyedContainer( player.getName(), block );
	
		// check for block relationships
		logBlockRelationshipsForBlock( player, block );
		
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockPlace(final BlockPlaceEvent event){
		Player player = event.getPlayer();
		plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_PLACE, event.getBlock(), player.getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockForm(BlockFormEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FORM, event.getBlock(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockFade(BlockFadeEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FADE, event.getBlock(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onLeavesDecay(LeavesDecayEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.LEAF_DECAY, event.getBlock(), "Environment") );
	}
	

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockBurn(BlockBurnEvent event) {
		Block block = event.getBlock();
		plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_BURN, block, "Environment") );
		
//		// check for block relationships
//		logBlockRelationshipsForBlock( player, block );
				
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
				if(plugin.preplannedBlockFalls.containsKey(coord_key)){
					String player = plugin.preplannedBlockFalls.get(coord_key);
					plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FALL, b, player) );
					plugin.preplannedBlockFalls.remove(coord_key);
				}
			}
		}
		
		// Log break of any attached items
		if (b.getState().getData() instanceof Attachable) {
			Attachable a = (Attachable)	b.getState().getData();
			if(a == null) return;
			Block attachedBlock = b.getRelative(a.getAttachedFace());
			// If it's lost an attached block
			if (attachedBlock.getTypeId() == 0) {
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				if(plugin.preplannedBlockFalls.containsKey(coord_key)){
					String player = plugin.preplannedBlockFalls.get(coord_key);
					plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_BREAK, b, player) );
					plugin.preplannedBlockFalls.remove(coord_key);
				}
			}
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onSignChange(SignChangeEvent event) {
		plugin.actionsRecorder.addToQueue( new SignAction(ActionType.SIGN_CHANGE, event.getBlock(), event.getLines(), event.getPlayer().getName()) );
	}


	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onSetFire(final BlockIgniteEvent event){
		
		ActionType cause = null;
		switch (event.getCause()){
			case FIREBALL:
				cause = ActionType.FIREBALL;
				break;
			case FLINT_AND_STEEL:
				cause = ActionType.FLINT_STEEL;
				break;
			case LAVA:
				cause = ActionType.LAVA_IGNITE;
				break;
			case LIGHTNING:
				cause = ActionType.LIGHTNING;
				break;
			case SPREAD:
//				cause = "fire-spread";
				break;
			default:
//				cause = "block-ignite";
		}
		if(cause != null){
			Player player = event.getPlayer();
			plugin.actionsRecorder.addToQueue( new BlockAction(cause, event.getBlock(), (player == null ? "Environment" : player.getName())) );
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerBucketEmpty(final PlayerBucketEmptyEvent event){
		Player player = event.getPlayer();
		ActionType cause = (event.getBucket() == Material.LAVA_BUCKET ? ActionType.LAVA_BUCKET : ActionType.WATER_BUCKET);
		plugin.actionsRecorder.addToQueue( new BlockAction(cause, event.getBlockClicked().getRelative(event.getBlockFace()), player.getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPistonExtend(final BlockPistonExtendEvent event){
//		if(event.getLength() > 0){
//			List<Block> blocks = event.getBlocks();
//			for( Block block : blocks){
//				// @todo record block move events
//			}
//		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPistonRetract(final BlockPistonRetractEvent event){
//		Block block = event.getBlock();
//		BlockFace face = event.getDirection();
//		event.getRetractLocation()
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockFromTo(BlockFromToEvent event) {

		// Ignore blocks that aren't liquid. @todo what else triggers this?
		if (!event.getBlock().isLiquid()) return;
		
		BlockState from = event.getBlock().getState();
		BlockState to = event.getToBlock().getState();

		// Lava
		if( from.getType().equals(Material.STATIONARY_LAVA) && to.getType().equals(Material.STATIONARY_WATER) ) {
			Block newTo = event.getToBlock();
			newTo.setType(Material.STONE);
			plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FORM, newTo, "Environment") );
		}

		// Water flowing into lava forms obsidian or cobble
		else if ( from.getType().equals(Material.WATER) || from.getType().equals(Material.STATIONARY_WATER) ) {
			BlockState lower = event.getToBlock().getRelative(BlockFace.DOWN).getState();
			if( lower.getType().equals(Material.COBBLESTONE) || lower.getType().equals(Material.OBSIDIAN) ){
				plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FORM, lower.getBlock(), "Environment") );
			}
		}
	}
}