package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.concurrent.LinkedBlockingQueue;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksRollbackEvent;
import me.botsko.prism.utils.EntityUtils;
import me.botsko.prism.wands.RollbackWand;
import me.botsko.prism.wands.Wand;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;

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
	protected final CommandSender sender;
	
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
	protected ArrayList<Handler> deferredChanges = new ArrayList<Handler>();
	
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
	protected final LinkedBlockingQueue<Handler> worldChangeQueue = new LinkedBlockingQueue<Handler>();
	
	/**
	 * 
	 */
	protected int worldChangeQueueTaskId;
	
	/**
	 * 
	 */
	protected ApplierCallback callback;
	
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Preview( Prism plugin, CommandSender sender, PrismProcessType processType, List<Handler> results, QueryParameters parameters, ApplierCallback callback ){
		this.processType = processType;
		this.plugin = plugin;
		this.sender = sender;
		this.parameters = parameters;
		
		if( sender instanceof Player ){
			this.player = (Player) sender;
		} else {
			this.player = null;
		}
		
		if(callback != null){
			this.callback = callback;
		}
		
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
		if( player == null ) return;
		if(!blockStateChanges.isEmpty()){
			for(BlockStateChange u : blockStateChanges){
				player.sendBlockChange(u.getOriginalBlock().getLocation(), u.getOriginalBlock().getTypeId(), u.getOriginalBlock().getRawData());
			}
		}
		sender.sendMessage( Prism.messenger.playerHeaderMsg( "Preview canceled." + ChatColor.GRAY + " Please come again!" ) );
	}
	
	
	/**
	 * 
	 */
	public void apply_preview(){
		if( player == null ) return;
		sender.sendMessage( Prism.messenger.playerHeaderMsg("Applying rollback from preview...") );
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
			
			if(!is_preview && player != null ){
				
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
	public void processWorldChanges(){
		worldChangeQueueTaskId = plugin.getServer().getScheduler().scheduleSyncRepeatingTask(plugin, new Runnable() {
			
		    public void run(){
		    	
		    	if(plugin.getConfig().getBoolean("prism.debug")){
		    		plugin.debug("World change queue size: " + worldChangeQueue.size() );
		    	}
		    	
				if(worldChangeQueue.isEmpty()){
					sender.sendMessage( Prism.messenger.playerError( ChatColor.GRAY + "No actions found that match the criteria." ) );
					return;
				}
		    	
		    	int iterationCount = 0;
				for (Iterator<Handler> iterator = worldChangeQueue.iterator(); iterator.hasNext(); ) {
					Handler a = iterator.next();
		    		
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
						

					/**
					 * Reverse or restore block changes by allowing the handler
					 * to decide what needs to happen.
					 */
					ChangeResult result = null;
					if(processType.equals(PrismProcessType.ROLLBACK)){
						result = a.applyRollback( player, parameters, is_preview );
					}
					if(processType.equals(PrismProcessType.RESTORE)){
						result = a.applyRestore( player, parameters, is_preview );
					}
					if(processType.equals(PrismProcessType.UNDO)){
						result = a.applyUndo( player, parameters, is_preview );
					}
					
					// No action, continue
					if( result == null ){
						worldChangeQueue.remove(a);
						continue;
					}
					// Deferring attachments/etc until required blocks are ready
					else if(result.getType().equals(ChangeResultType.DEFERRED)){
						deferredChanges.add( a );
					}
					// Skipping
					else if(result.getType().equals(ChangeResultType.SKIPPED)){
						skipped_block_count++;
						worldChangeQueue.remove(a);
						continue;
					}
					// Change applied
					else {
						blockStateChanges.add( result.getBlockStateChange() );
						changes_applied_count++;
					}
					// Unless a preview, remove from queue
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
		fireApplierCallback();
	}
	
	
	/**
	 * 
	 * @return
	 */
	public void postProcess(){
		
		// Apply deferred block changes
		for(Handler a : deferredChanges){
			
			ChangeResult result = a.applyDeferred( player, parameters, is_preview );
			// No action, continue
			if( result == null ){
				worldChangeQueue.remove(a);
				continue;
			}
			// Deferring attachments/etc until required blocks are ready
			else if(result.getType().equals(ChangeResultType.DEFERRED)){
				deferredChanges.add( a );
			}
			// Skipping
			else if(result.getType().equals(ChangeResultType.SKIPPED)){
				skipped_block_count++;
				worldChangeQueue.remove(a);
				continue;
			}
			// Change applied
			else {
				blockStateChanges.add( result.getBlockStateChange() );
				changes_applied_count++;
			}
		}
		
		// POST ROLLBACK TRIGGERS
		if(processType.equals(PrismProcessType.ROLLBACK)){
		
			// We're going to modify the action type of the query params
			// and pass it along to a restore.
			// NOTE: These params have been modified from original, so
			// do NOT use the object for original params.
			

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
		
		fireApplierCallback();
		
	}
	
	
	/**
	 * 
	 */
	protected void moveEntitiesToSafety(){
		if( parameters.getWorld() != null && player != null ){
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
								((Player)entity).sendMessage(Prism.messenger.playerSubduedHeaderMsg("Moved you " + add + " blocks to safety due to a rollback."));
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
	public void fireApplierCallback(){
		
		if(callback != null){
			callback.handle(sender, new ApplierResult(is_preview, changes_applied_count, skipped_block_count, blockStateChanges, processType));
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
