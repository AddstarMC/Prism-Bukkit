package me.botsko.prism.listeners;

import java.util.ArrayList;
import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.BlockShiftAction;
import me.botsko.prism.actions.GrowAction;
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
import org.bukkit.event.block.BlockSpreadEvent;
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
	
//	/**
//	 * We have to track the coords a blockfromto event triggers
//	 * a block form for, because it likes to fire several times
//	 * for the same block. It's a pretty silly thing.
//	 */
//	protected ArrayList<String> coordsUsed = new ArrayList<String>();
	
	
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
					Prism.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_REMOVE, i, i.getAmount(), block.getLocation(), player_name) );
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
					Prism.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_REMOVE, i, i.getAmount(), block.getLocation(), player_name) );
				}
			}
		}
	}
	
	
	/**
	 * 
	 * @param block
	 */
	public Block properlyLogDoubleLengthBlocks( Block block ){
		/**
		 * Handle special double-length blocks
		 */
		if( block.getType().equals(Material.WOODEN_DOOR) || block.getType().equals(Material.IRON_DOOR_BLOCK) ){
			// If you've broken the top half of a door, we need to record the action for the bottom.
			// This is because a top half break doesn't record the orientation of the door while the bottom does,
			// and we have code in the rollback/restore to add the top half back in.
			if(block.getData() == 8 || block.getData() == 9){
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
		return block;
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 */
	protected void logBlockRelationshipsForBlock( String playername, Block block ){
		
		if( block.getType().equals(Material.WOODEN_DOOR) || block.getType().equals(Material.IRON_DOOR_BLOCK) ){
			return;
		}
		
		// Find a list of all blocks above this block that we know
		// will fall. 
		ArrayList<Block> falling_blocks = BlockUtils.findFallingBlocksAboveBlock(block);
		if(falling_blocks.size() > 0){
			for(Block b : falling_blocks){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
//				plugin.debug("Anticipating falling block at " + coord_key + " for " + playername);
				plugin.preplannedBlockFalls.put(coord_key, playername);
			}
		}
		
//		// We can't have attachments on attachments so we'll end here.
//		if(BlockUtils.isDetachableBlock(block)){
//			return;
//		}
		
		
		// Find a list of side-face attached blocks that we expect will detach
		ArrayList<Block> detached_blocks = BlockUtils.findSideFaceAttachedBlocks(block);
		if(detached_blocks.size() > 0){
			for(Block b : detached_blocks){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
//				plugin.debug("Anticipating block detaching (side) at " + coord_key + " for " + playername);
				plugin.preplannedBlockFalls.put(coord_key, playername);
			}
		}
		
		// Find a list of top-side attached blocks that we expect will detach
		detached_blocks = BlockUtils.findTopFaceAttachedBlocks(block);
		if(detached_blocks.size() > 0){
			for(Block b : detached_blocks){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
//				plugin.debug("Anticipating block popping off (above) at " + coord_key + " for " + playername);
				plugin.preplannedBlockFalls.put(coord_key, playername);
//				plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_BREAK, block, playername) );
			}
		}
		
		// Find a list of all hanging entities on this block
		ArrayList<Entity> hanging = BlockUtils.findHangingEntities(block);
		if(hanging.size() > 0){
			for(Entity e : hanging){
				String coord_key = e.getLocation().getBlockX() + ":" + e.getLocation().getBlockY() + ":" + e.getLocation().getBlockZ();
//				plugin.debug("Anticipating hanging item detaching at " + coord_key + " for " + playername);
				plugin.preplannedBlockFalls.put(coord_key, playername);
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
		
		// Change handling a bit if it's a long block
		block = properlyLogDoubleLengthBlocks(block);
		
		Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_BREAK, block, player.getName()) );
		
		// log items removed from chest
		logItemRemoveFromDestroyedContainer( player.getName(), block );
	
		// check for block relationships
		logBlockRelationshipsForBlock( player.getName(), block );
		
		// if obsidian, log portal blocks
		if(block.getType().equals(Material.OBSIDIAN)){
			ArrayList<Block> blocks = BlockUtils.findConnectedBlocksOfType(Material.PORTAL, block, null);
			if(!blocks.isEmpty()){
				// Only log 1 portal break, we don't need all 8
				Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_BREAK, blocks.get(0), player.getName()) );
			}
		}
		
		// Pass to the break alerter
		plugin.useMonitor.alertOnBlockBreak(player, event.getBlock());
				
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockPlace(final BlockPlaceEvent event){
		
		Player player = event.getPlayer();
		Block block = event.getBlock();
		
		Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_PLACE, block, player.getName()) );
	
		// Pass to the placement alerter
		plugin.useMonitor.alertOnBlockPlacement(player, event.getBlock());
		
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockSpread(final BlockSpreadEvent event){
		if(event.getNewState().getType().equals(Material.FIRE)) return;
		Prism.actionsRecorder.addToQueue( new GrowAction(ActionType.BLOCK_SPREAD, event.getNewState(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockForm(final BlockFormEvent event) {
		Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FORM, event.getNewState(), null, "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockFade(final BlockFadeEvent event) {
		if(event.getBlock().getType().equals(Material.FIRE)) return;
		Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FADE, event.getBlock(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onLeavesDecay(final LeavesDecayEvent event) {
		Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.LEAF_DECAY, event.getBlock(), "Environment") );
	}
	

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockBurn(final BlockBurnEvent event) {
		Block block = event.getBlock();
		Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_BURN, block, "Environment") );
		
		// Change handling a bit if it's a long block
		block = properlyLogDoubleLengthBlocks(block);
		
		// check for block relationships
		logBlockRelationshipsForBlock( "Environment", block );
				
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockPhysics(final BlockPhysicsEvent event) {
		
		// Record that a block fell, associated with the player who broke the base block.
		Block b = event.getBlock();
		if(BlockUtils.isFallingBlock(b)){
			// Only record a block-fall if there's air below.
			if(b.getRelative(BlockFace.DOWN).getType().equals(Material.AIR)){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				if(plugin.preplannedBlockFalls.containsKey(coord_key)){
					String player = plugin.preplannedBlockFalls.get(coord_key);
					Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FALL, b, player) );
					plugin.preplannedBlockFalls.remove(coord_key);
				}
			}
		}
		
		// If it's an attachable item, we need to look for detachment
		// at the sides.
		// http://jd.bukkit.org/doxygen/d1/d0b/interfaceorg_1_1bukkit_1_1material_1_1Attachable.html#details
		if (b.getState().getData() instanceof Attachable) {
			Attachable a = (Attachable)	b.getState().getData();
			if(a == null) return;
			Block attachedBlock = b.getRelative(a.getAttachedFace());
			if(attachedBlock != null){
				// If it's lost an attached block
				if (BlockUtils.materialMeansBlockDetachment(attachedBlock.getType())) {
					String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
//					plugin.debug("Seeking block (side) detachment at: " + coord_key);
					if(plugin.preplannedBlockFalls.containsKey(coord_key)){
						String player = plugin.preplannedBlockFalls.get(coord_key);
						Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_BREAK, b, player) );
						plugin.preplannedBlockFalls.remove(coord_key);
					}
				}
			}
		} 
		// Otherwise we need to look for detachment at the bottom.
		else {
			
			Block attachedBlock = b.getRelative(BlockFace.DOWN);
			// If it's lost a supporting block
			if (BlockUtils.materialMeansBlockDetachment(attachedBlock.getType())) {
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
//				plugin.debug("Seeking block (below) detachment at: " + coord_key);
				if(plugin.preplannedBlockFalls.containsKey(coord_key)){
					String player = plugin.preplannedBlockFalls.get(coord_key);
					Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_BREAK, b, player) );
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
	public void onSignChange(final SignChangeEvent event) {
		Prism.actionsRecorder.addToQueue( new SignAction(ActionType.SIGN_CHANGE, event.getBlock(), event.getLines(), event.getPlayer().getName()) );
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
				cause = ActionType.LIGHTER;
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
			
			if( cause.equals(ActionType.LIGHTER) && plugin.getConfig().getBoolean("prism.alerts.uses.lighter") ){
				plugin.useMonitor.alertOnItemUse(player,"used a lighter");
			}
			
			Prism.actionsRecorder.addToQueue( new BlockAction(cause, event.getBlock(), (player == null ? "Environment" : player.getName())) );
			
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
		Prism.actionsRecorder.addToQueue( new BlockAction(cause, event.getBlockClicked().getRelative(event.getBlockFace()), player.getName()) );
		
		if(plugin.getConfig().getBoolean("prism.alerts.uses.lava") && event.getBucket() == Material.LAVA_BUCKET){
			plugin.useMonitor.alertOnItemUse(player,"poured lava");
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPistonExtend(final BlockPistonExtendEvent event){

		List<Block> blocks = event.getBlocks();
		if(!blocks.isEmpty()){
			for( Block block : blocks){
				
				if(block.getType().equals(Material.AIR)) continue;

				// Pistons move blocks to the block next to them. If nothing is there it shows as air.
				// We should record the from coords, to coords, and block replaced, as well as the block moved.
				Prism.actionsRecorder.addToQueue( new BlockShiftAction(ActionType.BLOCK_SHIFT, block, block.getRelative(event.getDirection()).getLocation(), "Piston") );
				
			}
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPistonRetract(final BlockPistonRetractEvent event){
		if(!event.isSticky()) return;
		Block block = event.getBlock();
		if(block.getType().equals(Material.AIR)) return;
		Prism.actionsRecorder.addToQueue( new BlockShiftAction(ActionType.BLOCK_SHIFT, event.getRetractLocation().getBlock(), block.getRelative(event.getDirection()).getLocation(), "Piston") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockFromTo(final BlockFromToEvent event) {

		// Ignore blocks that aren't liquid. @todo what else triggers this?
		if (!event.getBlock().isLiquid()) return;
		
		BlockState from = event.getBlock().getState();
		BlockState to = event.getToBlock().getState();
		
		// Record water flow
		if(from.getType() == Material.STATIONARY_WATER || from.getType() == Material.WATER){
			Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.WATER_FLOW, event.getBlock(), "Water"));
		}
		
		// Record lava flow
		if(from.getType() == Material.STATIONARY_LAVA || from.getType() == Material.LAVA){
			Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.LAVA_FLOW, event.getBlock(), "Lava"));
		}
		
		// Watch for blocks that the liquid can break
		if(BlockUtils.canFlowBreakMaterial(to.getType())){
			if(from.getType() == Material.STATIONARY_WATER || from.getType() == Material.WATER){
				Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.WATER_BREAK, event.getToBlock(), "Water"));
			} else if(from.getType() == Material.STATIONARY_LAVA || from.getType() == Material.LAVA){
				Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.LAVA_BREAK, event.getToBlock(), "Lava"));
			}
		}
		
		/**
		 * Predict the forming of Stone, Obsidian, Cobblestone because of lava/water flowing
		 * into each other. Boy, I wish bukkit used block_form for this.
		 */

		// Lava flows to water. STONE forms
		if( from.getType().equals(Material.STATIONARY_LAVA) && to.getType().equals(Material.STATIONARY_WATER) ) {
			Block newTo = event.getToBlock();
			newTo.setType(Material.STONE);
			Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FORM, newTo, "Environment") );
		}
		
	
////		int id = event.getBlock().getTypeId();
//		
//		 // If moving to air
//		Block b = event.getToBlock();
//		if(b.getType().equals(Material.AIR)){
//
//		 	// formed sat/lava = cobble
//		 	// formed stationary_water = stone
//		 
//			 // Are we moving from a water block
//			Material fromM = event.getBlock().getType();
//			if(fromM.equals(Material.WATER) || fromM.equals(Material.STATIONARY_WATER)){
//				// Check all sides
//				for(BlockFace face : BlockFace.values()){
//					Block r = b.getRelative(face, 1);
//					// If the side is lava, cobble shall form.
//					// Note: if stationary_lava, stone will form. Seems to always be captured above.
//					if(r.getType().equals(Material.LAVA) || r.getType().equals(Material.STATIONARY_LAVA)){
//						String coordsKey = r.getX()+":"+r.getY()+":"+r.getZ();
//						 if(coordsUsed.contains(coordsKey)) continue;
//						 coordsUsed.add(coordsKey);
//						 plugin.debug("COBBLE FORMED " + r.getType().name());
////						 r.setType(Material.COBBLESTONE);
//						 plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FORM, r, "Environment") );
//					}
//				}
//			}
//		}
//		
//
//		// Water flowing into lava forms obsidian or cobble
//		if ( from.getType().equals(Material.STATIONARY_WATER) && to.getType().equals(Material.STATIONARY_LAVA) ) {
//			plugin.debug("FROM WATER to " + to.getType().name());
//			BlockState lower = event.getToBlock().getRelative(BlockFace.DOWN).getState();
//			// Obsidian can form below 
//			if( lower.getType().equals(Material.OBSIDIAN) ){
//				String coordsKey = lower.getX()+":"+lower.getY()+":"+lower.getZ();
//				if(coordsUsed.contains(coordsKey)) return;
//				// Add coords to list the event has already fired for
//				coordsUsed.add(coordsKey);
//				plugin.debug("COBBLE/OBY FORMED BELOW " + coordsKey);
//				plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FORM, lower.getBlock(), "Environment") );
//			}
//
//		}
	}
}