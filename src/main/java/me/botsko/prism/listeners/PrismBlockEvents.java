package me.botsko.prism.listeners;

import java.util.ArrayList;
import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.block.Jukebox;
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
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;
import org.bukkit.material.Attachable;
import org.bukkit.material.Sign;
import org.bukkit.craftbukkit.v1_6_R3.CraftWorld; // MCPC+

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
	 * If this is a container we need to trigger item removal for everything in it.
	 * It's important we record this *after* the block break so the log shows what
	 * really happened.
	 * @param player_name
	 * @param block
	 */
	public void logItemRemoveFromDestroyedContainer( String player_name, Block block ){
		if(block.getType().equals(Material.JUKEBOX)){
			Jukebox jukebox = (Jukebox) block.getState();
			Material playing = jukebox.getPlaying();
			if( playing == null || playing.equals(Material.AIR) ) return;
			ItemStack i = new ItemStack(jukebox.getPlaying(),1);
			Prism.actionsRecorder.addToQueue( ActionFactory.create("item-remove", i, i.getAmount(), 0, null, block.getLocation(), player_name) );
			return;
		}
		if( block.getState() instanceof InventoryHolder ){
			InventoryHolder container = (InventoryHolder) block.getState();
			int slot = 0;
			for( ItemStack i : container.getInventory().getContents()){
				// when double chests are broken, they record *all* contents
				// even though only half of the chest breaks.
				if( block.getType().equals(Material.CHEST) && slot > 26 ) break;
				// record item
				if(i != null){
					Prism.actionsRecorder.addToQueue( ActionFactory.create("item-remove", i, i.getAmount(), slot, null, block.getLocation(), player_name) );
				}
				slot++;
			}
		}
	}
	
	
	/**
	 * 
	 * @param playername
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
				plugin.preplannedBlockFalls.put(coord_key, playername);
			}
		}
		
		// Some blocks will essentially never have attachments - not
		// even worth spending time looking for them.
		// SUGAR CANE is not a solid but does have top face attached
		if( !block.getType().isSolid() && !block.getType().equals(Material.SUGAR_CANE_BLOCK) ){
			return;
		}
		
		// if it's a piston, the base will break without a physics events
		if( block.getType().equals(Material.PISTON_EXTENSION) || block.getType().equals(Material.PISTON_MOVING_PIECE) ){
			ArrayList<Block> pistonBases = BlockUtils.findSideFaceAttachedBlocks( block );
			if( pistonBases.size() > 0 ){
				for( Block p : pistonBases ){
					Prism.actionsRecorder.addToQueue( ActionFactory.create("block-break", p, playername) );
				}
			}
		}

		// Find a list of side-face attached blocks that we expect will detach
		ArrayList<Block> detached_blocks = BlockUtils.findSideFaceAttachedBlocks(block);
		if(detached_blocks.size() > 0){
			for(Block b : detached_blocks){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				plugin.preplannedBlockFalls.put(coord_key, playername);
			}
		}
		
		// Find a list of top-side attached blocks that we expect will detach
		detached_blocks = BlockUtils.findTopFaceAttachedBlocks(block);
		if(detached_blocks.size() > 0){
			for(Block b : detached_blocks){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				plugin.preplannedBlockFalls.put(coord_key, playername);
			}
		}
		
		// Find a list of all hanging entities on this block
		ArrayList<Entity> hanging = BlockUtils.findHangingEntities(block);
		if(hanging.size() > 0){
			for(Entity e : hanging){
				String coord_key = e.getLocation().getBlockX() + ":" + e.getLocation().getBlockY() + ":" + e.getLocation().getBlockZ();
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

		if( block.getType().equals(Material.AIR) ) return;
		
		// Run ore find alerts
		if( !player.hasPermission("prism.alerts.ores.ignore") && !player.hasPermission("prism.alerts.ignore")  ){
			plugin.oreMonitor.processAlertsFromBlock(player, block);
		}
		// MCPC+ - ignore banned break id's
		if( !Prism.getIgnore().event("block-break",player) || Prism.getIllegalBreakBlocks().contains( block.getTypeId())) return;
		
		// Change handling a bit if it's a long block
		Block sibling = BlockUtils.getSiblingForDoubleLengthBlock(block);
		if( sibling != null && !block.getType().equals(Material.CHEST) ){
			block = sibling;
		}
		
		// log items removed from container
		// note: done before the container so a "rewind" for rollback will work properly
		//logItemRemoveFromDestroyedContainer( player.getName(), block ); // MCPC+ - not needed since we record TE NBT which includes all inventory
		// MCPC+ start - compress TileEntity data and queue for insert into db
		String te_data = BlockUtils.compressTileEntityData(event.getBlock());
		if (te_data != null)
		{
			Prism.actionsRecorder.addToQueue( ActionFactory.create("block-break", block, player.getName(), te_data) );
		}
		else
		{
			Prism.actionsRecorder.addToQueue( ActionFactory.create("block-break", block, player.getName()));
		}
		// MCPC+ end
		// check for block relationships
		logBlockRelationshipsForBlock( player.getName(), block );
		
		// if obsidian, log portal blocks
		if(block.getType().equals(Material.OBSIDIAN)){
			ArrayList<Block> blocks = BlockUtils.findConnectedBlocksOfType(Material.PORTAL, block, null);
			if(!blocks.isEmpty()){
				// Only log 1 portal break, we don't need all 8
				Prism.actionsRecorder.addToQueue( ActionFactory.create("block-break", blocks.get(0), player.getName()) );
			}
		}
		
		// Pass to the break alerter
		if( !player.hasPermission("prism.alerts.use.break.ignore") && !player.hasPermission("prism.alerts.ignore") ){
			plugin.useMonitor.alertOnBlockBreak(player, event.getBlock());
		}	
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockPlace(final BlockPlaceEvent event){
		
		Player player = event.getPlayer();
		Block block = event.getBlock();
		
		if( !Prism.getIgnore().event("block-place",player) ) return;
		
		if( block.getType().equals(Material.AIR) ) return;
		
		BlockState s = event.getBlockReplacedState();
		Prism.actionsRecorder.addToQueue( ActionFactory.create("block-place", block.getLocation(), s.getTypeId(), s.getRawData(), block.getTypeId(), block.getData(), player.getName()) );
	
		// Pass to the placement alerter
		if( !player.hasPermission("prism.alerts.use.place.ignore") && !player.hasPermission("prism.alerts.ignore") ){
			plugin.useMonitor.alertOnBlockPlacement(player, block);
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockSpread(final BlockSpreadEvent event){
		
		// If fire, do we track fire spread? If not, do we track block-spread
		String type = "block-spread";
		if(event.getNewState().getType().equals(Material.FIRE)){
			if( !Prism.getIgnore().event("fire-spread") ) return;
			type = "fire-spread";
		} else {
			if( !Prism.getIgnore().event("block-spread", event.getBlock()) ) return;
		}
		
		Block b = event.getBlock();
		BlockState s = event.getNewState();
		Prism.actionsRecorder.addToQueue( ActionFactory.create(type, b.getLocation(), b.getTypeId(), b.getData(), s.getTypeId(), s.getRawData(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockForm(final BlockFormEvent event) {
		if( !Prism.getIgnore().event("block-form", event.getBlock()) ) return;
		Block b = event.getBlock();
		BlockState s = event.getNewState();
		Prism.actionsRecorder.addToQueue( ActionFactory.create("block-form", b.getLocation(), b.getTypeId(), b.getData(), s.getTypeId(), s.getRawData(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockFade(final BlockFadeEvent event) {
		if( !Prism.getIgnore().event("block-fade", event.getBlock()) ) return;
		Block b = event.getBlock();
		if( b.getType().equals(Material.FIRE) ) return;
		BlockState s = event.getNewState();
		Prism.actionsRecorder.addToQueue( ActionFactory.create("block-fade", b.getLocation(), b.getTypeId(), b.getData(), s.getTypeId(), s.getRawData(), "Environment") );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onLeavesDecay(final LeavesDecayEvent event) {
		if( !Prism.getIgnore().event("leaf-decay", event.getBlock()) ) return;
		Prism.actionsRecorder.addToQueue( ActionFactory.create("leaf-decay", event.getBlock(), "Environment") );
	}
	

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockBurn(final BlockBurnEvent event) {
		if( !Prism.getIgnore().event("block-burn", event.getBlock()) ) return;
		Block block = event.getBlock();
		Prism.actionsRecorder.addToQueue( ActionFactory.create("block-burn", block, "Environment") );
		
		// Change handling a bit if it's a long block
		Block sibling = BlockUtils.getSiblingForDoubleLengthBlock(block);
		if( sibling != null && !block.getType().equals(Material.CHEST) ){
			block = sibling;
		}
		
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
			if( !Prism.getIgnore().event("block-fall", event.getBlock()) ) return;
			// Only record a block-fall if there's air below.
			if(b.getRelative(BlockFace.DOWN).getType().equals(Material.AIR)){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				if(plugin.preplannedBlockFalls.containsKey(coord_key)){
					String player = plugin.preplannedBlockFalls.get(coord_key);
					Prism.actionsRecorder.addToQueue( ActionFactory.create("block-fall", b, player) );
					plugin.preplannedBlockFalls.remove(coord_key);
				}
			}
		}
		// MCPC+ - ignore banned break id's
		if( !Prism.getIgnore().event("block-break", event.getBlock()) || !Prism.getIllegalBreakBlocks().contains( event.getBlock().getTypeId())) return;
		
		// If it's an attachable item, we need to look for detachment
		// at the sides.
		// http://jd.bukkit.org/doxygen/d1/d0b/interfaceorg_1_1bukkit_1_1material_1_1Attachable.html#details
		if (b.getState().getData() instanceof Attachable) {
			Attachable a = (Attachable)	b.getState().getData();
			if(a == null) return;
			if(a.getAttachedFace() == null) return;
			Block attachedBlock = b.getRelative(a.getAttachedFace());
			if(attachedBlock != null){
				// If it's lost an attached block
				if (BlockUtils.materialMeansBlockDetachment(attachedBlock.getType())) {
					String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
					if(plugin.preplannedBlockFalls.containsKey(coord_key)){
						String player = plugin.preplannedBlockFalls.get(coord_key);
						Prism.actionsRecorder.addToQueue( ActionFactory.create("block-break", b, player) );
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
				if(plugin.preplannedBlockFalls.containsKey(coord_key)){
					String player = plugin.preplannedBlockFalls.get(coord_key);
					Prism.actionsRecorder.addToQueue( ActionFactory.create("block-break", b, player) );
					plugin.preplannedBlockFalls.remove(coord_key);
				}
			}
			// MCPC+ start - temporary workaround for lack of break events in Forge
			Block block = event.getBlock();
			if (!((CraftWorld)block.getWorld()).getHandle().isEmpty(block.getX(), block.getY(), block.getZ()) && !Prism.getIllegalPhysicsBlocks().contains( block.getTypeId()))
			{
				String te_data = BlockUtils.compressTileEntityData(event.getBlock());
				if (te_data != null)
				{
					Prism.actionsRecorder.addToQueue( ActionFactory.create("block-break", block, "", te_data) );
				}
				else { 
					Prism.actionsRecorder.addToQueue( ActionFactory.create("block-break", block, ""));
				}
			}
			// MCPC+ end
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onSignChange(final SignChangeEvent event) {
		if( !Prism.getIgnore().event("sign-change", event.getPlayer()) ) return;
		if( event.getBlock().getState().getData() instanceof Sign ){
			Prism.actionsRecorder.addToQueue( ActionFactory.create("sign-change", event.getBlock(), event.getLines(), event.getPlayer().getName()) );
		}
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
				cause = "lighter";
				break;
			case LAVA:
				cause = "lava-ignite";
				break;
			case LIGHTNING:
				cause = "lightning";
				break;
			default:
		}
		if(cause != null){
			
			if( !Prism.getIgnore().event(cause, event.getBlock().getWorld()) ) return;
			
			Player player = event.getPlayer();

			if( player != null ){
				if( cause.equals("lighter") && plugin.getConfig().getBoolean("prism.alerts.uses.lighter") && !player.hasPermission("prism.alerts.use.lighter.ignore") && !player.hasPermission("prism.alerts.ignore") ){
					plugin.useMonitor.alertOnItemUse(player,"used a lighter");
				}
			}
			
			Prism.actionsRecorder.addToQueue( ActionFactory.create(cause, event.getBlock(), (player == null ? "Environment" : player.getName())) );
	
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPistonExtend(final BlockPistonExtendEvent event){
		
		if(plugin.getConfig().getBoolean("prism.alerts.vanilla-xray.enabled")){
			Block noPlayer = event.getBlock().getRelative( event.getDirection() ).getRelative( event.getDirection() ).getRelative(BlockFace.DOWN);
			for(Player pl : plugin.getServer().getOnlinePlayers()){
				Location loc = pl.getLocation();
				if( loc.getBlockX() == noPlayer.getX() && loc.getBlockY() == noPlayer.getY() && loc.getBlockZ() == noPlayer.getZ() ){
					plugin.useMonitor.alertOnVanillaXray(pl,"possibly used a vanilla piston/xray trick");
					break;
				}
			}
		}
		
		if( !Prism.getIgnore().event("block-shift", event.getBlock()) ) return;

		List<Block> blocks = event.getBlocks();
		if(!blocks.isEmpty()){
			for( Block block : blocks){
				
				if(block.getType().equals(Material.AIR)) continue;

				// Pistons move blocks to the block next to them. If nothing is there it shows as air.
				// We should record the from coords, to coords, and block replaced, as well as the block moved.
				Prism.actionsRecorder.addToQueue( ActionFactory.create("block-shift", block, block.getRelative(event.getDirection()).getLocation(), "Piston") );
				
			}
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPistonRetract(final BlockPistonRetractEvent event){
		if( !Prism.getIgnore().event("block-shift", event.getBlock()) ) return;
		if(!event.isSticky()) return;
		Block block = event.getBlock();
		if(block.getType().equals(Material.AIR)) return;
		Prism.actionsRecorder.addToQueue( ActionFactory.create("block-shift", event.getRetractLocation().getBlock(), block.getRelative(event.getDirection()).getLocation(), "Piston") );
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
		
		// Watch for blocks that the liquid can break
		if(BlockUtils.canFlowBreakMaterial(to.getType())){
			if(from.getType() == Material.STATIONARY_WATER || from.getType() == Material.WATER){
				if( Prism.getIgnore().event("water-break", event.getBlock()) ){
					Prism.actionsRecorder.addToQueue( ActionFactory.create("water-break", event.getToBlock(), "Water"));
				}
			} else if(from.getType() == Material.STATIONARY_LAVA || from.getType() == Material.LAVA){
				if( Prism.getIgnore().event("lava-break", event.getBlock()) ){
					Prism.actionsRecorder.addToQueue( ActionFactory.create("lava-break", event.getToBlock(), "Lava"));
				}
			}
		}
		
		// Record water flow
		if(from.getType() == Material.STATIONARY_WATER || from.getType() == Material.WATER){
			if( Prism.getIgnore().event("water-flow", event.getBlock()) ){
				Prism.actionsRecorder.addToQueue( ActionFactory.create("water-flow", event.getBlock(), "Water"));
			}
		}
		
		// Record lava flow
		if(from.getType() == Material.STATIONARY_LAVA || from.getType() == Material.LAVA){
			if( Prism.getIgnore().event("lava-flow", event.getBlock()) ){
				Prism.actionsRecorder.addToQueue( ActionFactory.create("lava-flow", event.getBlock(), "Lava"));
			}
		}
		
		/**
		 * Predict the forming of Stone, Obsidian, Cobblestone because of lava/water flowing
		 * into each other. Boy, I wish bukkit used block_form for this.
		 */
		if( !Prism.getIgnore().event("block-form", event.getBlock()) ) return;

		// Lava flows to water. STONE forms
		if( from.getType().equals(Material.STATIONARY_LAVA) && to.getType().equals(Material.STATIONARY_WATER) ) {
			Block newTo = event.getToBlock();
			newTo.setType(Material.STONE);
			Prism.actionsRecorder.addToQueue( ActionFactory.create("block-form", newTo, "Environment") );
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
//						 Prism.debug("COBBLE FORMED " + r.getType().name());
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
//			Prism.debug("FROM WATER to " + to.getType().name());
//			BlockState lower = event.getToBlock().getRelative(BlockFace.DOWN).getState();
//			// Obsidian can form below 
//			if( lower.getType().equals(Material.OBSIDIAN) ){
//				String coordsKey = lower.getX()+":"+lower.getY()+":"+lower.getZ();
//				if(coordsUsed.contains(coordsKey)) return;
//				// Add coords to list the event has already fired for
//				coordsUsed.add(coordsKey);
//				Prism.debug("COBBLE/OBY FORMED BELOW " + coordsKey);
//				plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_FORM, lower.getBlock(), "Environment") );
//			}
//
//		}
	}
}