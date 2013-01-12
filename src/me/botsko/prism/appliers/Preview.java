package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksRollbackEvent;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.EntityUtils;
import me.botsko.prism.wands.RollbackWand;
import me.botsko.prism.wands.Wand;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.block.Chest;
import org.bukkit.block.Sign;
import org.bukkit.entity.Ageable;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.entity.Sheep;
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
	protected final long processStartTime;
	
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
	public Preview( Prism plugin, Player player, PrismProcessType processType, List<Action> results, QueryParameters parameters, long processStartTime ){
		this.processType = processType;
		this.plugin = plugin;
		this.player = player;
		this.parameters = parameters;
		this.processStartTime = processStartTime;
		
		// Append all actions to the queue.
		// @todo Is there a better way to append these to the queue?
		for(Action a : results){
			worldChangeQueue.add(a);
		}
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
		
		Material m = Material.getMaterial(b.getBlock_id());
		
		// We're doing a rollback, we need to ensure the location we're replacing doesn't
		// have a new block already.
		if( processType.equals(PrismProcessType.ROLLBACK) && !BlockUtils.isAcceptableForBlockPlace(block) ){
			return ChangeResultType.SKIPPED;
		}
		
		// On the blacklist?
		if( !BlockUtils.mayEverPlace(m) ){
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
			
			// Set the material
			block.setTypeId( b.getBlock_id() );
			block.setData( b.getBlock_subid() );
			
			// Capture the new state
			BlockState newBlock = block.getState();
			
			// Store the state change
			blockStateChanges.add( new BlockStateChange(originalBlock,newBlock) );
			
			// If we're rolling back a door, we need to set it properly
			if( m.equals(Material.WOODEN_DOOR) || m.equals(Material.IRON_DOOR_BLOCK) ){
				BlockUtils.properlySetDoor( block, b.getBlock_id(), b.getBlock_subid());
			}
			// Or a bed
			if( m.equals(Material.BED_BLOCK) ){
				BlockUtils.properlySetBed( block, b.getBlock_id(), b.getBlock_subid());
			}
		} else {
			
			// Otherwise, save the state so we can cancel if needed
			BlockState originalBlock = block.getState();
			// Note: we save the original state as both old/new so we can re-use blockStateChanges
			blockStateChanges.add( new BlockStateChange(originalBlock,originalBlock) );
			
			// Preview it
			player.sendBlockChange(block.getLocation(), b.getBlock_id(), b.getBlock_subid());
			
		}
		
		return ChangeResultType.APPLIED;

	}


	/***
	 * 
	 */
	protected ChangeResultType removeBlock( Block block ){
		
		// @todo ensure we're not removing a new block that's been placed by someone else
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
					 * Rollback entity kills
					 */
					if( processType.equals(PrismProcessType.ROLLBACK) && a instanceof EntityAction ){
						
						EntityAction b = (EntityAction) a;
						
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
						
						changes_applied_count++;
						
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
		if(is_preview && changes_applied_count > 0){
			// Append the preview and blocks temporarily
			PreviewSession ps = new PreviewSession( player, this );
			plugin.playerActivePreviews.put(player.getName(), ps);
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
			BlockAction b = (BlockAction) a;
			World world = plugin.getServer().getWorld(b.getWorld_name());
			Location loc = new Location(world, b.getX(), b.getY(), b.getZ());
			Block block = world.getBlockAt(loc);
			placeBlock( b, block, true );
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
						Restore rs = new Restore( plugin, player, PrismProcessType.RESTORE, results.getActionResults(), triggerParameters, processStartTime );
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
		
		
		// Make sure we move the player out of the way
		for(Player player : plugin.getServer().getWorld(parameters.getWorld()).getPlayers()){
			int add = 0;
			if(EntityUtils.inCube(parameters.getPlayerLocation(), parameters.getRadius(), player.getLocation())){
				Location l = player.getLocation();
				while( !EntityUtils.playerMayPassThrough(l.getBlock().getType()) ){
					add++;
					if(l.getY() >= 256) break;
					l.setY(l.getY() + 1);
				}
				if(add > 0){
					player.sendMessage(plugin.playerSubduedHeaderMsg("Moved you " + add + " blocks to safety due to a rollback."));
					player.teleport(l);
				}
			}
		}
		
		
		// Trigger the rollback event
		PrismBlocksRollbackEvent event = new PrismBlocksRollbackEvent(blockStateChanges, player, parameters.getOriginalCommand());
		plugin.getServer().getPluginManager().callEvent(event);
		
		sendResultMessages();
		
	}
	
	
	/**
	 * 
	 */
	public void sendResultMessages(){
		
		// Calc the final time
		Calendar lCDateTime = Calendar.getInstance();
		long processEndTime = lCDateTime.getTimeInMillis();
		long timeDiff = processEndTime - processStartTime;
		String timeTaken = "";
		if(plugin.getConfig().getBoolean("prism.debug")){
			timeTaken = is_preview ? "" : " ("+timeDiff+"ms)";
		}
		
		// Send player success messages
		if(processType.equals(PrismProcessType.ROLLBACK)){
		
			// Build the results message
			if(!is_preview){
				
				String msg = changes_applied_count + " reversals"+timeTaken+".";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(changes_applied_count > 0){
					msg += ChatColor.GRAY + " It's like it never happened.";
				}
				player.sendMessage( plugin.playerHeaderMsg( msg ) );
				
			} else {
			
				// Build the results message
				String msg = changes_applied_count + " planned reversals.";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(changes_applied_count > 0){
					msg += ChatColor.GRAY + " Use /prism preview apply to confirm this rollback.";
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
				String msg = changes_applied_count + " events restored"+timeTaken+".";
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
					msg += ChatColor.GRAY + " Use /prism preview apply to confirm this restore.";
				}
				player.sendMessage( plugin.playerHeaderMsg( msg ) );
				
				// Let me know there's no need to cancel/apply
				if(changes_applied_count == 0){
					player.sendMessage( plugin.playerHeaderMsg( ChatColor.GRAY + "Nothing to restore, preview canceled for you." ) );
				}
			}
		}
	}
}
