package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.Chest;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.EntityUtils;

public class Rollback extends Preview {
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Rollback( Prism plugin, Player player, List<Action> results, QueryParameters parameters ) {
		this.plugin = plugin;
		this.player = player;
		this.results = results;
		this.parameters = parameters;
	}
	
	
	/**
	 * Set preview move and then do a rollback
	 * @return
	 */
	public ApplierResult preview(){
		is_preview = true;
		return apply();
	}
	
	
	/**
	 * 
	 */
	public ApplierResult apply(){
		
		ArrayList<String> responses = new ArrayList<String>();
		int rolled_back_count = 0, skipped_block_count = 0;
		ArrayList<Undo> undo = new ArrayList<Undo>();
		
		// Remove any fire at this location
		if(plugin.getConfig().getBoolean("prism.appliers.remove-fire-on-burn-rollback") && parameters.getActionTypes().contains(ActionType.BLOCK_BURN)){
			int fires_ext = BlockUtils.extinguish(player.getLocation(),parameters.getRadius());
			if(fires_ext > 0){
				responses.add( plugin.playerHeaderMsg("Extinguishing fire!" + ChatColor.GRAY + " Like a boss.") );
			}
		}
		
		// Remove item drops in this radius
		if(plugin.getConfig().getBoolean("prism.appliers.remove-drops-on-explode-rollback") && (parameters.getActionTypes().contains(ActionType.TNT_EXPLODE) || parameters.getActionTypes().contains(ActionType.CREEPER_EXPLODE)) ){
			int removed = EntityUtils.removeNearbyItemDrops(player, parameters.getRadius());
			if(removed > 0){
				responses.add( plugin.playerHeaderMsg("Removed " + removed + " drops in affected area." + ChatColor.GRAY + " Like a boss.") );
			}
		}
		
//		// Remove any liquid at this location
//		if(plugin.getConfig().getBoolean("prism.appliers.remove-liquid-on-flow-rollback") && ( parameters.getActionTypes().contains(ActionType.WATER_FLOW) || parameters.getActionTypes().contains(ActionType.LAVA_FLOW)) ){
//			int fires_ext = BlockUtils.drain(player.getLocation(),parameters.getRadius());
//			if(fires_ext > 0){
//				responses.add( plugin.playerHeaderMsg("Draining liquid first!" + ChatColor.GRAY + " Like a boss.") );
//			}
//		}
		
		// can't really work here. doesn't return a proper result, etc
		// Remove any lava blocks when doing a lava bucket rollback
		if(parameters.getActionTypes().contains(ActionType.LAVA_BUCKET) || parameters.getActionTypes().contains(ActionType.LAVA_FLOW)){
			BlockUtils.drainlava(parameters.getPlayerLocation(), parameters.getRadius());
		}
		
		if(parameters.getActionTypes().contains(ActionType.WATER_BUCKET) || parameters.getActionTypes().contains(ActionType.WATER_FLOW)){
			BlockUtils.drainwater(parameters.getPlayerLocation(), parameters.getRadius());
		}
		
		// Rollback blocks
		if(!results.isEmpty()){
			
			ArrayList<BlockAction> reattach = new ArrayList<BlockAction>();
			
			for(Action a : results){
				
				// No sense in trying to rollback
				// when the type doesn't support it.
				if(!a.getType().isCanRollback()){
					continue;
				}
					
				
				World world = plugin.getServer().getWorld(a.getWorld_name());
				
				//Get some data from the entry
				Location loc = new Location(world, a.getX(), a.getY(), a.getZ());
				
				/**
				 * Rollback block changes
				 */
				if( a instanceof BlockAction ){
					
					BlockAction b = (BlockAction) a;
					Block block = world.getBlockAt(loc);
					
					if(is_preview){
						// Record the change temporarily so we can cancel
						// and update the client
						Undo u = new Undo( block );
						undo.add(u);
					}

					// If the block was placed, we need to remove it
					if(a.getType().doesCreateBlock()){
						// @todo ensure we're not removing a new block that's been placed by someone else
						if(!block.getType().equals(Material.AIR)){
							if(!is_preview){
								block.setType(Material.AIR);
							} else {
								player.sendBlockChange(block.getLocation(), Material.AIR, (byte)0);
							}
							rolled_back_count++;
						}
					} else {
						
						/**
						 * Restore the block that was removed, unless something
						 * other than air occupies the spot.
						 * 
						 * Restore the block if we detect a falling block now sits there. This resolves
						 * the issue of falling blocks taking up the space, preventing this rollback.
						 * However, it also means that a rollback *could* interfere with a player-placed
						 * block.
						 */
						if( BlockUtils.isAcceptableForBlockPlace(block) ){
							
							Material m = Material.getMaterial(b.getBlock_id());
							
							// Ignore any blocks we should never place
							if(!BlockUtils.mayEverPlace(m)){
								skipped_block_count++;
								continue;
							}
							
							// If it's attachable to the sides or top, we need to delay
							if( (BlockUtils.isSideFaceDetachableMaterial(m) || BlockUtils.isTopFaceDetachableMaterial(m)) && !BlockUtils.isDoor(m)){
								reattach.add(b);
								continue;
							}

							
							if(!is_preview){
								block.setTypeId( b.getBlock_id() );
								block.setData( b.getBlock_subid() );
								// If we're rolling back a door, we need to set it properly
								if( m.equals(Material.WOODEN_DOOR) || m.equals(Material.IRON_DOOR_BLOCK) ){
									BlockUtils.properlySetDoor( block, b.getBlock_id(), b.getBlock_subid());
								}
								// Or a bed
								if( m.equals(Material.BED_BLOCK) ){
									BlockUtils.properlySetBed( block, b.getBlock_id(), b.getBlock_subid());
								}
								
							} else {
								player.sendBlockChange(block.getLocation(), b.getBlock_id(), b.getBlock_subid());
							}
							
							rolled_back_count++;
							
						}
					}
				}
				
				
				/**
				 * Rollback entity kills
				 */
				if( a instanceof EntityAction ){
					
					EntityAction b = (EntityAction) a;
					
					if(!EntityUtils.mayEverSpawn(b.getEntityTypeFromData())){
						skipped_block_count++;
						continue;
					}
					
					world.spawnEntity(loc, b.getEntityTypeFromData());
					
					plugin.debug("Rolling back entity " + b.getEntityTypeFromData().getName());
					
					rolled_back_count++;
					
				}
				
				
				/**
				 * Rollback itemstack actions
				 */
				if( a instanceof ItemStackAction ){
					
					ItemStackAction b = (ItemStackAction) a;
					
					Block block = world.getBlockAt(loc);
					if(block.getType().equals(Material.CHEST)){
						Chest chest = (Chest) block.getState();
						
						// If item was removed, put it back.
						if(a.getType().equals(ActionType.ITEM_REMOVE) && plugin.getConfig().getBoolean("prism.appliers.allow_rollback_items_removed_from_container")){
							HashMap<Integer,ItemStack> leftovers = chest.getInventory().addItem( b.getItem() );
							rolled_back_count++;
							if(leftovers.size() > 0){
								plugin.debug("Couldn't rollback items to container, it's full.");
							}
						}
					}
				}
			}
			
			
			/**
			 * Lets loop through the block actions with detachable blocks
			 * after we have put back other blocks so they can reattach.
			 */
			for(BlockAction b : reattach){
				
				World world = plugin.getServer().getWorld(b.getWorld_name());
				Location loc = new Location(world, b.getX(), b.getY(), b.getZ());
				Block block = world.getBlockAt(loc);
				
				if(!is_preview){
					block.setTypeId( b.getBlock_id() );
					block.setData( b.getBlock_subid() );
				} else {
					player.sendBlockChange(block.getLocation(), b.getBlock_id(), b.getBlock_subid());
				}
				
				rolled_back_count++;
			}
			
			
			// POST ROLLBACK TRIGGERS
			
			// We're going to modify the action type of the query params
			// and pass it along to a restore.
			// NOTE: These params have been modified from original, so
			// do NOT use the object for original params.
			
			/**
			 * If we've done breaking-blocks rollback we also need to re-apply
			 * any sign-change events at this location.
			 */
			if(parameters.shouldTriggerRestoreFor(ActionType.SIGN_CHANGE)){
				
				QueryParameters triggerParameters;
				try {
					triggerParameters = parameters.clone();
					triggerParameters.resetActionTypes();
					triggerParameters.addActionType(ActionType.SIGN_CHANGE);
					
					ActionsQuery aq = new ActionsQuery(plugin);
					QueryResult results = aq.lookup( player, triggerParameters );
					if(!results.getActionResults().isEmpty()){
						Restore rs = new Restore( plugin, player, results.getActionResults(), triggerParameters );
						rs.apply();
					}
				} catch (CloneNotSupportedException e) {
					e.printStackTrace();
				}
			}
			
			
			/**
			 * If we've rolled back any containers we need to restore item-removes.
			 */
			if(parameters.shouldTriggerRollbackFor(ActionType.ITEM_REMOVE)){
				
				plugin.debug("Action being rolled back triggers a second rollback: Item Remove");
				
				QueryParameters triggerParameters;
				try {
					triggerParameters = parameters.clone();
					triggerParameters.resetActionTypes();
					triggerParameters.addActionType(ActionType.ITEM_REMOVE);
					
					ActionsQuery aq = new ActionsQuery(plugin);
					QueryResult results = aq.lookup( player, triggerParameters );
					if(!results.getActionResults().isEmpty()){
						Rollback rb = new Rollback( plugin, player, results.getActionResults(), triggerParameters );
						rb.apply();
					}
				} catch (CloneNotSupportedException e) {
					e.printStackTrace();
				}
			}
			
			
			// Build the results message
			if(!is_preview){
				
				String msg = rolled_back_count + " reversals.";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(rolled_back_count > 0){
					msg += ChatColor.GRAY + " It's like it never happened.";
				}
				responses.add( plugin.playerHeaderMsg( msg ) );
				
				for(Player player : plugin.getServer().getWorld(parameters.getWorld()).getPlayers()){
					int add = 0;
					if(EntityUtils.inCube(parameters.getPlayerLocation(), parameters.getRadius(), player.getLocation())){
						Location l = player.getLocation();
						while(l.getBlock().getType() != Material.AIR){
							add++;
							l.setY(l.getY() + 1);
						}
						if(add > 0){
							player.sendMessage(plugin.playerHeaderMsg("A rollback was performed near you, so to prevent suffocation, you have been moved upward " + add + " blocks."));
							player.teleport(l);
						}
					}
				}
				
			} else {
			
				// Build the results message
				String msg = rolled_back_count + " planned reversals.";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(rolled_back_count > 0){
					msg += ChatColor.GRAY + " Use /prism preview apply to confirm this rollback.";
				}
				player.sendMessage( plugin.playerHeaderMsg( msg ) );
				
				// Let me know there's no need to cancel/apply
				if(rolled_back_count == 0){
					player.sendMessage( plugin.playerHeaderMsg( ChatColor.GRAY + "Nothing to rollback, preview canceled for you." ) );
				}
			}
		} else {
			responses.add( plugin.playerError( "Nothing found to rollback. Try using /prism l (args) first." ) );
		}
		return new ApplierResult( is_preview, rolled_back_count, skipped_block_count, undo, responses );
	}
}