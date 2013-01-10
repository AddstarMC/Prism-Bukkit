package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

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
import me.botsko.prism.changers.BlockChangeResult;
import me.botsko.prism.changers.ChangeResultType;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksRollbackEvent;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.EntityUtils;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.block.Chest;
import org.bukkit.block.Sign;
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
	PrismProcessType processType;
	
	/**
	 * 
	 */
	protected Player player;
	
	/**
	 * 
	 */
	protected List<Action> results;
	
	/**
	 * 
	 */
	protected QueryParameters parameters;
	
	/**
	 * 
	 */
	protected boolean is_preview = false;
	
	/**
	 * 
	 */
	int skipped_block_count;
	
	/**
	 * 
	 */
	int changes_applied_count;
	
	
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
		if(plugin.playerActivePreviews.containsKey(player.getName())){
			
			PreviewSession previewSession = plugin.playerActivePreviews.get( player.getName() );
			plugin.debug("Undo queue empty: " + previewSession.getResults().getBlockStateChanges().isEmpty());
			if(!previewSession.getResults().getBlockStateChanges().isEmpty()){
				
				for(BlockStateChange u : previewSession.getResults().getBlockStateChanges()){
					plugin.debug("Preview block sent to player: " + u.getOriginalBlock().getType().name());
					player.sendBlockChange(u.getOriginalBlock().getLocation(), u.getOriginalBlock().getTypeId(), u.getOriginalBlock().getRawData());
				}
			}
			
			player.sendMessage( plugin.playerHeaderMsg( "Preview canceled." + ChatColor.GRAY + " Please come again!" ) );
			
			plugin.playerActivePreviews.remove( player.getName() );
			
		}
	}
	
	
	/**
	 * 
	 */
	public void apply_preview(){
		if(plugin.playerActivePreviews.containsKey(player.getName())){
			
			// Get preview session
			PreviewSession ps = plugin.playerActivePreviews.get(player.getName());
			
			player.sendMessage( plugin.playerHeaderMsg("Applying rollback from preview...") );
			ps.getPreviewer().setIsPreview(false);
			ps.getPreviewer().apply();
			
			plugin.playerActivePreviews.remove( player.getName() );
			
		}
	}
	

	/**
	 * 
	 */
	public ApplierResult preview() {
		return null;
	}


	/**
	 * 
	 * @return
	 */
	public ApplierResult apply(){
		
		ArrayList<Action> deferredChanges = new ArrayList<Action>();
		ArrayList<BlockStateChange> blockStateChanges = new ArrayList<BlockStateChange>();
		
		if(results != null && !results.isEmpty()){
			
			for(Action a : results){;
				
				// No sense in trying to rollback
				// when the type doesn't support it.
				if( processType.equals(PrismProcessType.ROLLBACK) && !a.getType().canRollback()){
					continue;
				}
				
				// No sense in trying to restore
				// when the type doesn't support it.
				if( processType.equals(PrismProcessType.RESTORE) && !a.getType().canRestore()){
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
					BlockChangeResult result = applyBlockChange( (BlockAction) a, loc.getWorld().getBlockAt(loc) );
					
					if(result.getChangeResultType().equals(ChangeResultType.DEFERRED)){
						deferredChanges.add( a );
					}
					else if(result.getChangeResultType().equals(ChangeResultType.SKIPPED)){
						skipped_block_count++;
						continue;
					} else {
						blockStateChanges.add(result.getBlockStateChange());
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
						continue;
					}
					
					Entity entity = world.spawnEntity(loc, b.getEntityType());
					
					// Set sheep color
					if( entity.getType().equals(EntityType.SHEEP)){
						Sheep sheep = ((Sheep) entity);
						sheep.setColor( b.getColor() );
					}
					
					plugin.debug("Rolling back entity " + b.getEntityType().getName());
					
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
								plugin.debug("Couldn't rollback items to container, it's full.");
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
			}
			
			// Apply deferred block changes
			for(Action a : deferredChanges){
				
				BlockAction b = (BlockAction) a;
				
				World world = plugin.getServer().getWorld(b.getWorld_name());
				Location loc = new Location(world, b.getX(), b.getY(), b.getZ());
				Block block = world.getBlockAt(loc);
				
				if(!is_preview){
					block.setTypeId( b.getBlock_id() );
					block.setData( b.getBlock_subid() );
				} else {
					player.sendBlockChange(block.getLocation(), b.getBlock_id(), b.getBlock_subid());
				}
				
				changes_applied_count++;
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
			
			return new ApplierResult( is_preview, changes_applied_count, skipped_block_count, blockStateChanges );
			
		}
		return null;
	}
	
	
	/**
	 * 
	 */
	protected BlockChangeResult applyBlockChange(BlockAction b, Block block){
		
		// Rollbacks remove blocks players created, and restore blocks player's removed
		if(processType.equals(PrismProcessType.ROLLBACK)){
			if(b.getType().doesCreateBlock()){
				return removeBlock(block);
			} else {
				return placeBlock(b,block);
			}
		}
		
		// Restores break blocks players placed again, and re-place blocks player's have placed before
		if(processType.equals(PrismProcessType.RESTORE)){
			if(b.getType().doesCreateBlock()){
				return placeBlock(b,block);
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
	protected BlockChangeResult placeBlock( BlockAction b, Block block ){
		
		Material m = Material.getMaterial(b.getBlock_id());
		BlockStateChange blockStateChanges = null;
		
		// We're doing a rollback, we need to ensure the location we're replacing doesn't
		// have a new block already.
		if( processType.equals(PrismProcessType.ROLLBACK) && !BlockUtils.isAcceptableForBlockPlace(block) ){
			return new BlockChangeResult( ChangeResultType.SKIPPED, null );
		}
		
		// On the blacklist?
		if( !BlockUtils.mayEverPlace(m) ){
			return new BlockChangeResult( ChangeResultType.SKIPPED, null );
		}
			
		// If it's attachable to the sides or top, we need to delay
		if( (BlockUtils.isSideFaceDetachableMaterial(m) || BlockUtils.isTopFaceDetachableMaterial(m)) && !BlockUtils.isDoor(m)){
			return new BlockChangeResult( ChangeResultType.DEFERRED, null );
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
			blockStateChanges = new BlockStateChange(originalBlock,newBlock);
			
			// If we're rolling back a door, we need to set it properly
			if( m.equals(Material.WOODEN_DOOR) || m.equals(Material.IRON_DOOR_BLOCK) ){
				BlockUtils.properlySetDoor( block, b.getBlock_id(), b.getBlock_subid());
			}
			// Or a bed
			if( m.equals(Material.BED_BLOCK) ){
				BlockUtils.properlySetBed( block, b.getBlock_id(), b.getBlock_subid());
			}
		} else {
			
			// Otherwise, preview it.
			player.sendBlockChange(block.getLocation(), b.getBlock_id(), b.getBlock_subid());
		}
		
		return new BlockChangeResult( ChangeResultType.APPLIED, blockStateChanges );

	}


	/***
	 * 
	 */
	protected BlockChangeResult removeBlock( Block block ){
		
		BlockStateChange blockStateChanges = null;
		
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
				blockStateChanges = new BlockStateChange(originalBlock,newBlock);
				
			} else {
				player.sendBlockChange(block.getLocation(), Material.AIR, (byte)0);
			}
			return new BlockChangeResult( ChangeResultType.APPLIED, blockStateChanges );
		}
		return new BlockChangeResult( ChangeResultType.SKIPPED, null );
	}
}
