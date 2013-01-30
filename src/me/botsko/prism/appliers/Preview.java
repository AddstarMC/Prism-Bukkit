package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.concurrent.LinkedBlockingQueue;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.BlockAction.SkullActionData;
import me.botsko.prism.actions.BlockAction.SpawnerActionData;
import me.botsko.prism.actions.BlockChangeAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.HangingItemAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.actions.WorldeditAction;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksRollbackEvent;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.EntityUtils;
import me.botsko.prism.wands.RollbackWand;
import me.botsko.prism.wands.Wand;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.block.Chest;
import org.bukkit.block.CreatureSpawner;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;
import org.bukkit.entity.Ageable;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Hanging;
import org.bukkit.entity.ItemFrame;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Painting;
import org.bukkit.entity.Player;
import org.bukkit.entity.Sheep;
import org.bukkit.entity.Villager;
import org.bukkit.entity.Wolf;
import org.bukkit.inventory.ItemStack;

public class Preview implements Previewable {
	
	/**
	 * 
	 */
	protected Prism plugin;
	
	/**
	 * 
	 */
	protected final PrismProcessType processType;
	
	/**
	 * 
	 */
	protected final Player player;
	
	/**
	 * 
	 */
	protected final QueryParameters parameters;
	
	/**
	 * 
	 */
	protected boolean is_preview = false;
	
	/**
	 * 
	 */
	protected ArrayList<Action> deferredChanges = new ArrayList<Action>();
	
	/**
	 * 
	 */
	protected ArrayList<BlockStateChange> blockStateChanges = new ArrayList<BlockStateChange>();
	
	/**
	 * 
	 */
	protected int skipped_block_count;
	
	/**
	 * 
	 */
	protected int changes_applied_count;
	
	/**
	 * 
	 */
	protected final LinkedBlockingQueue<Action> worldChangeQueue = new LinkedBlockingQueue<Action>();
	
	/**
	 * 
	 */
	protected int worldChangeQueueTaskId;
	
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Preview( Prism plugin, Player player, PrismProcessType processType, List<Action> results, QueryParameters parameters ){
		this.processType = processType;
		this.plugin = plugin;
		this.player = player;
		this.parameters = parameters;
		
		// Append all actions to the queue.
		worldChangeQueue.addAll(results);
		
	}
	
	
	/**
	 * 
	 * @param is_preview
	 */
	public void setIsPreview(boolean is_preview){
		this.is_preview = is_preview;
	}
	
	
	/**
	 * 
	 */
	public void cancel_preview(){
		if(!blockStateChanges.isEmpty()){
			for(BlockStateChange u : blockStateChanges){
				player.sendBlockChange(u.getOriginalBlock().getLocation(), u.getOriginalBlock().getTypeId(), u.getOriginalBlock().getRawData());
			}
		}
		player.sendMessage( plugin.playerHeaderMsg( "Preview canceled." + ChatColor.GRAY + " Please come again!" ) );
	}
	
	
	/**
	 * 
	 */
	public void apply_preview(){
		player.sendMessage( plugin.playerHeaderMsg("Applying rollback from preview...") );
		setIsPreview(false);
		changes_applied_count = 0;
		skipped_block_count = 0;
		apply();
	}
	

	/**
	 * 
	 */
	public void preview() {}


	/**
	 * 
	 * @return
	 */
	public void apply(){
		
		if(!worldChangeQueue.isEmpty()){
			
			if(!is_preview){
				
				Wand oldwand = null;
				if(plugin.playersWithActiveTools.containsKey(player.getName())){
					// Pull the wand in use
					oldwand = plugin.playersWithActiveTools.get(player.getName());
				}
				
				boolean show_nearby = true;
				if(oldwand != null && oldwand instanceof RollbackWand){
					show_nearby = false;
				}
				if(show_nearby){
					// Inform nearby players
					plugin.notifyNearby(player, parameters.getRadius(), player.getDisplayName() + " is performing a " + processType.name().toLowerCase() + " near you.");
					// Inform staff
					if(plugin.getConfig().getBoolean("prism.alerts.alert-staff-to-applied-process")){
						String cmd = parameters.getOriginalCommand();
						if(cmd != null){
							plugin.alertPlayers( player, ChatColor.WHITE + processType.name().toLowerCase() + " by " + player.getDisplayName() + ChatColor.GRAY + parameters.getOriginalCommand() );
						}
					}
				}
			}
			
			// Offload the work of world changes
			// to a scheduled sync task
			processWorldChanges();
		}
	}
	
	
	/**
	 * 
	 */
	protected ChangeResultType applyBlockChange(BlockAction b, Block block){
		
		// Rollbacks remove blocks players created, and restore blocks player's removed
		if(processType.equals(PrismProcessType.ROLLBACK)){
			if(b.getType().doesCreateBlock()){
				return removeBlock(block);
			} else {
				return placeBlock(b,block,false);
			}
		}
		
		// Restores break blocks players placed again, and re-place blocks player's have placed before
		if(processType.equals(PrismProcessType.RESTORE)){
			if(b.getType().doesCreateBlock()){
				return placeBlock(b,block,false);
			} else {
				return removeBlock(block);
			}
		}
		
		// Undo a drain/ext event (which always remove blocks)
		// @todo if we ever track rollback/restore for undo, we'll
		// need logic to do the opposite
		if(processType.equals(PrismProcessType.UNDO)){
			return placeBlock(b,block,false);
		}
		
		return null;
	}
	
	
	/**
	 * 
	 */
	protected ChangeResultType applyBlockChange(BlockChangeAction b, Block block){
		
		BlockAction blockAction = new BlockAction(null, null, null);
		
		// Rollbacks will revert a block to the old state
		if(processType.equals(PrismProcessType.ROLLBACK)){
			blockAction.setBlockId( b.getActionData().old_id );
			blockAction.setBlockSubId( b.getActionData().old_subid );
			return placeBlock(blockAction,block,false);
		}
		
		// Restores a block to it's new state
		if(processType.equals(PrismProcessType.RESTORE)){
			blockAction.setBlockId( b.getActionData().new_id );
			blockAction.setBlockSubId( b.getActionData().new_subid );
			return placeBlock(blockAction,block,false);
		}
		
		return null;
	}
	
	
	/**
	 * Place a block unless something other than air occupies the spot, or if we detect 
	 * a falling block now sits there. This resolves
	 * the issue of falling blocks taking up the space, preventing this rollback.
	 * However, it also means that a rollback *could* interfere with a player-placed
	 * block.
	 */
	protected ChangeResultType placeBlock( final BlockAction b, Block block, boolean is_deferred ){
		
		Material m = Material.getMaterial(b.getActionData().getBlockId());
		
		if( parameters.hasFlag(Flag.NO_OVERWRITE) ){
			// We're doing a rollback, we need to ensure the location we're replacing doesn't
			// have a new block already.
			if( processType.equals(PrismProcessType.ROLLBACK) && !BlockUtils.isAcceptableForBlockPlace(block) ){
				return ChangeResultType.SKIPPED;
			}
		}
		
		// On the blacklist (except an undo)
		if( !BlockUtils.mayEverPlace(m) && !processType.equals(PrismProcessType.UNDO) ){
			return ChangeResultType.SKIPPED;
		}
			
		// If it's attachable to the sides or top, we need to delay unless we're handling those now
		if( !is_deferred && (BlockUtils.isSideFaceDetachableMaterial(m) || BlockUtils.isTopFaceDetachableMaterial(m))){
			return ChangeResultType.DEFERRED;
		}

		// If we're not in a preview, actually apply this block
		if(!is_preview){
			
			// Capture the block before we change it
			BlockState originalBlock = block.getState();

			// If lilypad, check that block below is water. Be sure
			// it's set to stationary water so the lilypad will sit
			if( b.getActionData().getBlockId() == 111 ){
				
				Block below = block.getRelative(BlockFace.DOWN);
				if( below.getType().equals(Material.WATER) || below.getType().equals(Material.AIR) || below.getType().equals(Material.STATIONARY_WATER) ){
					below.setType(Material.STATIONARY_WATER);
				} else {
					return ChangeResultType.SKIPPED;
				}
			}
			
			// If portal, we need to light the portal. seems to be the only way.
			if( b.getActionData().getBlockId() == 90 ){
				Block obsidian = BlockUtils.getFirstBlockOfMaterialBelow(Material.OBSIDIAN, block.getLocation());
				if(obsidian != null){
					Block above = obsidian.getRelative(BlockFace.UP);
					if(!above.equals(Material.PORTAL)){
						above.setType(Material.FIRE);
						return ChangeResultType.APPLIED;
					}
				}
			}
			
			// Set the material
			block.setTypeId( b.getActionData().getBlockId() );
			block.setData( b.getActionData().getBlockSubid() );
			
			
			/**
			 * Skulls
			 */
			if( b.getActionData().getBlockId() == 144 || b.getActionData().getBlockId() == 397 ){
				
				SkullActionData s = (SkullActionData) b.getActionData();
	
				// Set skull data
				Skull skull = (Skull) block.getState();
				skull.setRotation( s.getRotation() );
				skull.setSkullType( s.getSkullType() );
				skull.update();
				
			}
			
			
			/**
			 * Spawner
			 */
			if( b.getActionData().getBlockId() == 52 ){
				
				SpawnerActionData s = (SpawnerActionData) b.getActionData();
				
				// Set spawner data
				CreatureSpawner spawner = (CreatureSpawner) block.getState();
				spawner.setDelay(s.getDelay());
				spawner.setSpawnedType(s.getEntityType());
				spawner.update();
				
			}
			
			// If the material is a crop that needs soil, we must restore the soil
			// This may need to go before setting the block, but I prefer the BlockUtil
			// logic to use materials.
			if( BlockUtils.materialRequiresSoil(block.getType()) ){
				Block below = block.getRelative(BlockFace.DOWN);
				if( below.getType().equals(Material.DIRT) || below.getType().equals(Material.AIR) || below.getType().equals(Material.GRASS) ){
					below.setType(Material.SOIL);
				} else {
					return ChangeResultType.SKIPPED;
				}
			}
			
			// Capture the new state
			BlockState newBlock = block.getState();
			
			// Store the state change
			blockStateChanges.add( new BlockStateChange(originalBlock,newBlock) );
			
			// If we're rolling back a door, we need to set it properly
			if( m.equals(Material.WOODEN_DOOR) || m.equals(Material.IRON_DOOR_BLOCK) ){
				BlockUtils.properlySetDoor( block, b.getActionData().getBlockId(), b.getActionData().getBlockSubid());
			}
			// Or a bed
			else if( m.equals(Material.BED_BLOCK) ){
				BlockUtils.properlySetBed( block, b.getActionData().getBlockId(), b.getActionData().getBlockSubid());
			}
		} else {
			
			// Otherwise, save the state so we can cancel if needed
			BlockState originalBlock = block.getState();
			// Note: we save the original state as both old/new so we can re-use blockStateChanges
			blockStateChanges.add( new BlockStateChange(originalBlock,originalBlock) );
			
			// Preview it
			player.sendBlockChange(block.getLocation(), b.getActionData().getBlockId(), b.getActionData().getBlockSubid());
			
		}
		
		return ChangeResultType.APPLIED;

	}


	/***
	 * 
	 */
	protected ChangeResultType removeBlock( Block block ){
		
		if(!block.getType().equals(Material.AIR)){
			if(!is_preview){
				
				// Capture the block before we change it
				BlockState originalBlock = block.getState();
				
				// Set
				block.setType(Material.AIR);
				
				// Capture the new state
				BlockState newBlock = block.getState();
				
				// Store the state change
				blockStateChanges.add( new BlockStateChange(originalBlock,newBlock) );
				
			} else {
				
				// Otherwise, save the state so we can cancel if needed
				BlockState originalBlock = block.getState();
				// Note: we save the original state as both old/new so we can re-use blockStateChanges
				blockStateChanges.add( new BlockStateChange(originalBlock,originalBlock) );
				
				// Preview it
				player.sendBlockChange(block.getLocation(), Material.AIR, (byte)0);
				
			}
			return ChangeResultType.APPLIED;
		}
		return ChangeResultType.SKIPPED;
	}
	
	
	
	/**
	 * 
	 */
	public void processWorldChanges(){
		worldChangeQueueTaskId = plugin.getServer().getScheduler().scheduleSyncRepeatingTask(plugin, new Runnable() {
			
		    public void run() {
		    	
		    	if(plugin.getConfig().getBoolean("prism.debug")){
		    		plugin.debug("World change queue size: " + worldChangeQueue.size() );
		    	}
		    	
				if(worldChangeQueue.isEmpty()){
					player.sendMessage( plugin.playerError( ChatColor.GRAY + "No actions found that match the criteria." ) );
					return;
				}
		    	
		    	int iterationCount = 0;
	    		for (Iterator<Action> iterator = worldChangeQueue.iterator(); iterator.hasNext(); ) {
	    			Action a = iterator.next(); 
		    		
		    		// We only want to process a set number of block changes per 
		    		// schedule. Breaking here will leave the rest in the queue.
		    		iterationCount++;
		    		if(iterationCount >= 1000){
		    			break;
		    		}
					
					// No sense in trying to rollback
					// when the type doesn't support it.
					if( processType.equals(PrismProcessType.ROLLBACK) && !a.getType().canRollback()){
						worldChangeQueue.remove(a);
						continue;
					}
					
					// No sense in trying to restore
					// when the type doesn't support it.
					if( processType.equals(PrismProcessType.RESTORE) && !a.getType().canRestore()){
						worldChangeQueue.remove(a);
						continue;
					}
						
					// Determine the location
					World world = plugin.getServer().getWorld(a.getWorld_name());
					Location loc = new Location(world, a.getX(), a.getY(), a.getZ());
		
					
					/**
					 * Reverse or restore block changes
					 */
					if( a instanceof BlockAction ){
						
						// Pass along to the change handler
						ChangeResultType result = applyBlockChange( (BlockAction) a, loc.getWorld().getBlockAt(loc) );
						
						if(result.equals(ChangeResultType.DEFERRED)){
							deferredChanges.add( a );
						}
						else if(result.equals(ChangeResultType.SKIPPED)){
							skipped_block_count++;
							worldChangeQueue.remove(a);
							continue;
						} else {
							changes_applied_count++;
						}
					}
					
					
					/**
					 * Reverse or restore block state changes
					 */
					if( a instanceof BlockChangeAction ){
						
						// Pass along to the change handler
						ChangeResultType result = applyBlockChange( (BlockChangeAction) a, loc.getWorld().getBlockAt(loc) );
						
						if(result.equals(ChangeResultType.DEFERRED)){
							deferredChanges.add( a );
						}
						else if(result.equals(ChangeResultType.SKIPPED)){
							skipped_block_count++;
							worldChangeQueue.remove(a);
							continue;
						} else {
							changes_applied_count++;
						}
					}
					
					
					/**
					 * Rollback entity kills
					 */
					if( processType.equals(PrismProcessType.ROLLBACK) && a instanceof EntityAction ){
						
						EntityAction b = (EntityAction) a;
						
						if(b.getEntityType() == null){
							skipped_block_count++;
							worldChangeQueue.remove(a);
							continue;
						}
						
						if(!EntityUtils.mayEverSpawn(b.getEntityType())){
							skipped_block_count++;
							worldChangeQueue.remove(a);
							continue;
						}
						
						Entity entity = world.spawnEntity(loc, b.getEntityType());
						
						// Get animal age
						if(entity instanceof Ageable){
							Ageable age = (Ageable)entity;
							if(!b.isAdult()){
								age.setBaby();
							}
						}
						
						// Set sheep color
						if( entity.getType().equals(EntityType.SHEEP)){
							Sheep sheep = ((Sheep) entity);
							sheep.setColor( b.getColor() );
						}
						
						// Set villager profession
						if( entity instanceof Villager ){
							Villager v = (Villager)entity;
							v.setProfession( b.getProfession() );
						}
						
						// Set taming owner
						if (entity instanceof Wolf){
				            Wolf wolf = (Wolf)entity;
				            Player owner = plugin.getServer().getPlayer( b.getTamingOwner() );
				            if(owner == null){
					            OfflinePlayer offlinePlayer = plugin.getServer().getOfflinePlayer(b.getTamingOwner());
					            if(offlinePlayer.hasPlayedBefore()){
					            	owner = (Player)offlinePlayer;
					            }
				            }
				            if(owner != null) wolf.setOwner(owner);
				    	}
						
						changes_applied_count++;
						
					}
					
					
					/**
					 * Hanging items
					 */
					if( a instanceof HangingItemAction){
						deferredChanges.add( a );
					}
					
					
					/**
					 * Rollback itemstack actions
					 */
					if( processType.equals(PrismProcessType.ROLLBACK) && a instanceof ItemStackAction ){
						
						ItemStackAction b = (ItemStackAction) a;
						
						Block block = world.getBlockAt(loc);
						if(block.getType().equals(Material.CHEST)){
							Chest chest = (Chest) block.getState();
							
							// If item was removed, put it back.
							if(a.getType().equals(ActionType.ITEM_REMOVE) && plugin.getConfig().getBoolean("prism.appliers.allow_rollback_items_removed_from_container")){
								HashMap<Integer,ItemStack> leftovers = chest.getInventory().addItem( b.getItem() );
								changes_applied_count++;
								if(leftovers.size() > 0){
									// @todo
								}
							}
						}
					}
					

					/**
					 * Restore sign actions
					 */
					if( processType.equals(PrismProcessType.RESTORE) && a instanceof SignAction ){
						
						SignAction b = (SignAction) a;
						Block block = world.getBlockAt(loc);
						
						// Ensure a sign exists there (and no other block)
						if( block.getType().equals(Material.AIR) || block.getType().equals(Material.SIGN_POST) || block.getType().equals(Material.SIGN) || block.getType().equals(Material.WALL_SIGN) ){
							
							if( block.getType().equals(Material.AIR) ){
								block.setType(b.getSignType());
							}
							
							// Set the facing direction
							Sign s = (Sign)block.getState();
							
							if(block.getType().equals(Material.SIGN)){
								((org.bukkit.material.Sign)s.getData()).setFacingDirection(b.getFacing());
							}
							
							// Set content
							String[] lines = b.getLines();
							int i = 0;
							if(lines.length > 0){
								for(String line : lines){
									s.setLine(i, line);
									i++;
								}
							}
							s.update();
							changes_applied_count++;
						}
					}
					
					
					/**
					 * R/R of worldedit changes
					 */
					if( a instanceof WorldeditAction ){
						
						WorldeditAction b = (WorldeditAction) a;
						Block block = world.getBlockAt(loc);
						
						// Rollback replaces block with OLD block
						if( processType.equals(PrismProcessType.ROLLBACK) ){
							block.setTypeId(b.getOriginalBlockId());
							block.setData( (byte)b.getOriginalBlockSubId() );
						}
						
						// Restore replaces block with NEW block
						if( processType.equals(PrismProcessType.RESTORE) ){
							block.setTypeId(b.getNewBlockId());
							block.setData( (byte)b.getNewBlockSubId() );
						}
						
						changes_applied_count++;

					}
					
					if(!is_preview){
						worldChangeQueue.remove(a);
					}
				}
	    		
	    		// The task for this action is done being used
	    		if(worldChangeQueue.isEmpty() || is_preview){
	    			plugin.getServer().getScheduler().cancelTask(worldChangeQueueTaskId);
	    		}
	    		
		    	// If the queue is empty, we're done
		    	if(worldChangeQueue.isEmpty()){
		    		postProcess();
		    	} else {
		    		
		    		// otherwise we're previewing and need
		    		// info about the planned rollback
		    		if(is_preview){
		    			postProcessPreview();
		    		}
		    	}
		    }
		}, 2L, 2L);
	}
	
	
	/**
	 * Store the preview session for later use
	 */
	public void postProcessPreview(){
		// Count how many time 
		changes_applied_count += deferredChanges.size();
		if(is_preview && changes_applied_count > 0){
			// Append the preview and blocks temporarily
			PreviewSession ps = new PreviewSession( player, this );
			plugin.playerActivePreviews.put(player.getName(), ps);
			moveEntitiesToSafety();
		}
		sendResultMessages();
	}
	
	
	/**
	 * 
	 * @return
	 */
	public void postProcess(){
		
		// Apply deferred block changes
		for(Action a : deferredChanges){
			
			if( a instanceof HangingItemAction){
				
				HangingItemAction h = (HangingItemAction)a;
				
				BlockFace attachedFace = h.getDirection().getOppositeFace();
				World world = plugin.getServer().getWorld(h.getWorld_name());
				Location loc = new Location(world, h.getX(), h.getY(), h.getZ()).getBlock().getRelative(h.getDirection()).getLocation();
				
				// bug filed:
				// https://bukkit.atlassian.net/browse/BUKKIT-3371
				if( h.getHangingType().equals("item_frame") ){
					Hanging hangingItem = world.spawn(loc, ItemFrame.class);
					hangingItem.setFacingDirection( attachedFace, true );
					changes_applied_count++;
				}
				else if( h.getHangingType().equals("painting") ){
					Hanging hangingItem = world.spawn(loc, Painting.class);
					hangingItem.setFacingDirection( h.getDirection(), true );
					changes_applied_count++;
				}
			} else {
			
				BlockAction b = null;
				if(a instanceof BlockChangeAction){
					BlockChangeAction bc = (BlockChangeAction) a;
					b = new BlockAction(null, null, null);
					b.setWorld_name(bc.getWorld_name());
					b.setX( bc.getX() );
					b.setY( bc.getY() );
					b.setZ( bc.getZ() );
					if(processType.equals(PrismProcessType.ROLLBACK)){
						b.setBlockId( bc.getActionData().old_id );
						b.setBlockSubId( bc.getActionData().old_subid );
					} else {
						b.setBlockId( bc.getActionData().new_id );
						b.setBlockSubId( bc.getActionData().new_subid );
					}
				} else {
					b = (BlockAction) a;
				}
			
				World world = plugin.getServer().getWorld(b.getWorld_name());
				Location loc = new Location(world, b.getX(), b.getY(), b.getZ());
				Block block = world.getBlockAt(loc);
				ChangeResultType res = placeBlock( b, block, true );
				if(res.equals(ChangeResultType.APPLIED)){
					changes_applied_count++;
				}
				else if(res.equals(ChangeResultType.SKIPPED)){
					skipped_block_count++;
				} else {
					plugin.log("Error: Deferred block placement encountered an additional deferral for block "+block.getTypeId()+":"+block.getData()+". Report to Prism developers.");
				}
			}
		}
		
		// POST ROLLBACK TRIGGERS
		if(processType.equals(PrismProcessType.ROLLBACK)){
		
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
						Restore rs = new Restore( plugin, player, PrismProcessType.RESTORE, results.getActionResults(), triggerParameters );
						rs.apply();
					}
				} catch (CloneNotSupportedException e) {
					e.printStackTrace();
				}
			}
			
		
//			/**
//			 * If we've rolled back any containers we need to restore item-removes.
//			 */
//			if(parameters.shouldTriggerRollbackFor(ActionType.ITEM_REMOVE)){
//				
//				plugin.debug("Action being rolled back triggers a second rollback: Item Remove");
//				
//				QueryParameters triggerParameters;
//				try {
//					triggerParameters = parameters.clone();
//					triggerParameters.resetActionTypes();
//					triggerParameters.addActionType(ActionType.ITEM_REMOVE);
//					
//					ActionsQuery aq = new ActionsQuery(plugin);
//					QueryResult results = aq.lookup( player, triggerParameters );
//					if(!results.getActionResults().isEmpty()){
//						Rollback rb = new Rollback( plugin, player, results.getActionResults(), triggerParameters );
//						rb.apply();
//					}
//				} catch (CloneNotSupportedException e) {
//					e.printStackTrace();
//				}
//			}
		}
		
		moveEntitiesToSafety();
		
		// Trigger the rollback event
		PrismBlocksRollbackEvent event = new PrismBlocksRollbackEvent(blockStateChanges, player, parameters.getOriginalCommand());
		plugin.getServer().getPluginManager().callEvent(event);
		
		sendResultMessages();
		
	}
	
	
	/**
	 * 
	 */
	protected void moveEntitiesToSafety(){
		if( parameters.getWorld() != null ){
			List<Entity> entities = player.getNearbyEntities(parameters.getRadius(), parameters.getRadius(), parameters.getRadius());
			entities.add((Entity)player);
			for(Entity entity : entities){
				if(entity instanceof LivingEntity){
					int add = 0;
					if(EntityUtils.inCube(parameters.getPlayerLocation(), parameters.getRadius(), entity.getLocation())){
						Location l = entity.getLocation();
						while( !EntityUtils.playerMayPassThrough(l.getBlock().getType()) ){
							add++;
							if(l.getY() >= 256) break;
							l.setY(l.getY() + 1);
						}
						if(add > 0){
							if(entity instanceof Player){
								((Player)entity).sendMessage(plugin.playerSubduedHeaderMsg("Moved you " + add + " blocks to safety due to a rollback."));
							}
							entity.teleport(l);
						}
					}
				}
			}
		}
	}
	
	
	/**
	 * 
	 */
	public void sendResultMessages(){
		
		// Send player success messages
		if(processType.equals(PrismProcessType.ROLLBACK)){
		
			// Build the results message
			if(!is_preview){
				
				String msg = changes_applied_count + " reversals.";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(changes_applied_count > 0){
					msg += ChatColor.GRAY + " It's like it never happened.";
				}
				player.sendMessage( plugin.playerHeaderMsg( msg ) );
				
			} else {
			
				// Build the results message
				String msg = "At least " + changes_applied_count + " planned reversals.";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(changes_applied_count > 0){
					msg += ChatColor.GRAY + " Use /prism preview apply to confirm.";
				}
				player.sendMessage( plugin.playerHeaderMsg( msg ) );
				
				// Let me know there's no need to cancel/apply
				if(changes_applied_count == 0){
					player.sendMessage( plugin.playerHeaderMsg( ChatColor.GRAY + "Nothing to rollback, preview canceled for you." ) );
				}
			}
		}
		
		
		// Build the results message
		if(processType.equals(PrismProcessType.RESTORE)){
			if(!is_preview){
				
				// Build the results message
				String msg = changes_applied_count + " events restored.";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(changes_applied_count > 0){
					msg += ChatColor.GRAY + " It's like it was always there.";
				}
				player.sendMessage( plugin.playerHeaderMsg( msg ) );
				
			} else {
			
				// Build the results message
				String msg = changes_applied_count + " planned restorations.";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(changes_applied_count > 0){
					msg += ChatColor.GRAY + " Use /prism preview apply to confirm.";
				}
				player.sendMessage( plugin.playerHeaderMsg( msg ) );
				
				// Let me know there's no need to cancel/apply
				if(changes_applied_count == 0){
					player.sendMessage( plugin.playerHeaderMsg( ChatColor.GRAY + "Nothing to restore, preview canceled for you." ) );
				}
			}
		}
		
		
		// Build the results message
		if(processType.equals(PrismProcessType.UNDO)){
				
			// Build the results message
			String msg = changes_applied_count + " things neverminded.";
			if(skipped_block_count > 0){
				msg += " " + skipped_block_count + " skipped.";
			}
			if(changes_applied_count > 0){
				msg += ChatColor.GRAY + " If anyone asks, you never did that.";
			}
			player.sendMessage( plugin.playerHeaderMsg( msg ) );
			
		}
		
		plugin.eventTimer.recordTimedEvent("applier function complete");
		
		// record timed events to log
		if(plugin.getConfig().getBoolean("prism.debug")){
			TreeMap<Long,String> timers = plugin.eventTimer.getEventsTimedList();
			if(timers.size() > 0){
				long lastTime = 0;
				long total = 0;
				plugin.debug("-- Timer information for last action: --");
				for (Entry<Long, String> entry : timers.entrySet()){
					long diff = 0;
					if(lastTime > 0){
						diff = entry.getKey() - lastTime;
						total += diff;
					}
					plugin.debug(entry.getValue() + " " + diff + "ms");
					lastTime = entry.getKey();
				}
				plugin.debug("Total time: " + total + "ms");
				plugin.debug("Changes: " + changes_applied_count);
				plugin.debug("Skipped: " + skipped_block_count);
			}
		}
		plugin.eventTimer.resetEventList();
	}
}
