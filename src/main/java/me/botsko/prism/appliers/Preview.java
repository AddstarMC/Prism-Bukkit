package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import me.botsko.elixr.EntityUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksRollbackEvent;
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
    protected final Prism plugin;

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
    protected final HashMap<Entity, Integer> entities_moved = new HashMap<Entity, Integer>();

    /**
	 * 
	 */
    protected final ArrayList<BlockStateChange> blockStateChanges = new ArrayList<BlockStateChange>();

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
    protected int changes_planned_count;

    /**
     * Keep a count of changes read from the queue. We don't actually remove the
     * changes from the queue if this is a preview, so we need to know when
     * we've reached the end.
     */
    protected int blockChangesRead = 0;

    /**
	 * 
	 */
    protected final List<Handler> worldChangeQueue = Collections.synchronizedList( new LinkedList<Handler>() );

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
    public Preview(Prism plugin, CommandSender sender, List<Handler> results, QueryParameters parameters,
            ApplierCallback callback) {

        this.processType = parameters.getProcessType();
        this.plugin = plugin;
        this.sender = sender;
        this.parameters = parameters;

        if( sender instanceof Player ) {
            this.player = (Player) sender;
        } else {
            this.player = null;
        }

        if( callback != null ) {
            this.callback = callback;
        }

        // Append all actions to the queue.
        worldChangeQueue.addAll( results );

    }

    /**
     * 
     * @param is_preview
     */
    @Override
    public void setIsPreview(boolean is_preview) {
        this.is_preview = is_preview;
    }

    /**
	 * 
	 */
    @Override
    public void cancel_preview() {
        if( player == null )
            return;
        if( !blockStateChanges.isEmpty() ) {

            // pull all players that are part of this preview
            final ArrayList<CommandSender> previewPlayers = parameters.getSharedPlayers();
            previewPlayers.add( player );

            for ( final BlockStateChange u : blockStateChanges ) {
                for ( final CommandSender sharedPlayer : previewPlayers ) {
                    if( sharedPlayer instanceof Player )
                        ( (Player) sharedPlayer ).sendBlockChange( u.getOriginalBlock().getLocation(), u
                                .getOriginalBlock().getTypeId(), u.getOriginalBlock().getRawData() );
                }
            }
        }
        sender.sendMessage( Prism.messenger.playerHeaderMsg( "Preview canceled." + ChatColor.GRAY
                + " Please come again!" ) );
    }

    /**
	 * 
	 */
    @Override
    public void apply_preview() {
        if( player == null )
            return;
        sender.sendMessage( Prism.messenger.playerHeaderMsg( "Applying rollback from preview..." ) );
        setIsPreview( false );
        changes_applied_count = 0;
        skipped_block_count = 0;
        changes_planned_count = 0;
        apply();
    }

    /**
	 * 
	 */
    @Override
    public void preview() {}

    /**
     * 
     * @return
     */
    @Override
    public void apply() {

        if( !worldChangeQueue.isEmpty() ) {

            if( !is_preview && player != null ) {

                Wand oldwand = null;
                if( Prism.playersWithActiveTools.containsKey( player.getName() ) ) {
                    // Pull the wand in use
                    oldwand = Prism.playersWithActiveTools.get( player.getName() );
                }

                boolean show_nearby = true;
                if( oldwand != null && oldwand instanceof RollbackWand ) {
                    show_nearby = false;
                }
                if( show_nearby ) {
                    // Inform nearby players
                    plugin.notifyNearby( player, parameters.getRadius(), player.getDisplayName() + " is performing a "
                            + processType.name().toLowerCase() + " near you." );
                    // Inform staff
                    if( plugin.getConfig().getBoolean( "prism.alerts.alert-staff-to-applied-process" ) ) {
                        final String cmd = parameters.getOriginalCommand();
                        if( cmd != null ) {
                            plugin.alertPlayers( player, ChatColor.WHITE + processType.name().toLowerCase() + " by "
                                    + player.getDisplayName() + ChatColor.GRAY + parameters.getOriginalCommand() );
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
    public void processWorldChanges() {

        blockChangesRead = 0;

        worldChangeQueueTaskId = plugin.getServer().getScheduler().scheduleSyncRepeatingTask( plugin, new Runnable() {

            @Override
            public void run() {

                if( plugin.getConfig().getBoolean( "prism.debug" ) ) {
                    Prism.debug( "World change queue size: " + worldChangeQueue.size() );
                }

                if( worldChangeQueue.isEmpty() ) {
                    sender.sendMessage( Prism.messenger.playerError( ChatColor.GRAY
                            + "No actions found that match the criteria." ) );
                    return;
                }

                int iterationCount = 0;
                final int currentQueueOffset = blockChangesRead;
                if( currentQueueOffset < worldChangeQueue.size() ) {
                    for ( final Iterator<Handler> iterator = worldChangeQueue.listIterator( currentQueueOffset ); iterator
                            .hasNext(); ) {
                        final Handler a = iterator.next();

                        // Only iterate the queue using a diff offset when
                        // previewing, actual rollbacks
                        // will remove from the queue and we'd begin at zero
                        // again
                        if( is_preview )
                            blockChangesRead++;

                        // We only want to process a set number of block changes
                        // per
                        // schedule. Breaking here will leave the rest in the
                        // queue.
                        iterationCount++;
                        if( iterationCount >= 1000 ) {
                            break;
                        }

                        // No sense in trying to rollback
                        // when the type doesn't support it.
                        if( processType.equals( PrismProcessType.ROLLBACK ) && !a.getType().canRollback() ) {
                            iterator.remove();
                            continue;
                        }

                        // No sense in trying to restore
                        // when the type doesn't support it.
                        if( processType.equals( PrismProcessType.RESTORE ) && !a.getType().canRestore() ) {
                            iterator.remove();
                            continue;
                        }

                        /**
                         * Reverse or restore block changes by allowing the
                         * handler to decide what needs to happen.
                         */
                        ChangeResult result = null;

                        try {
                            if( processType.equals( PrismProcessType.ROLLBACK ) ) {
                                result = a.applyRollback( player, parameters, is_preview );
                            }
                            if( processType.equals( PrismProcessType.RESTORE ) ) {
                                result = a.applyRestore( player, parameters, is_preview );
                            }
                            if( processType.equals( PrismProcessType.UNDO ) ) {
                                result = a.applyUndo( player, parameters, is_preview );
                            }

                            // No action, continue
                            if( result == null ) {
                                iterator.remove();
                                continue;
                            }
                            // Skip actions that have not returned any results
                            if( result.getType() == null ) {
                                skipped_block_count++;
                                iterator.remove();
                                continue;
                            }
                            // Skipping
                            else if( result.getType().equals( ChangeResultType.SKIPPED ) ) {
                                skipped_block_count++;
                                iterator.remove();
                                continue;
                            }
                            // Skipping, but change planned
                            else if( result.getType().equals( ChangeResultType.PLANNED ) ) {
                                changes_planned_count++;
                                continue;
                            }
                            // Change applied
                            else {
                                blockStateChanges.add( result.getBlockStateChange() );
                                changes_applied_count++;
                            }
                            // Unless a preview, remove from queue
                            if( !is_preview ) {
                                iterator.remove();
                            }
                        } catch ( final Exception e ) {

                            // Something caused an exception. We *have* to catch
                            // this
                            // so we can remove the item from the queue,
                            // otherwise
                            // the cycle will just spin in eternity and all
                            // damnation, normally killing their server through
                            // log files in the GB.

                            // Log the error so they have something to report
                            Prism.log( "Applier error: " + e.getMessage() );
                            e.printStackTrace();

                            // Count as skipped, remove from queue
                            skipped_block_count++;
                            iterator.remove();

                        }
                    }
                }

                // The task for this action is done being used
                if( worldChangeQueue.isEmpty() || blockChangesRead >= worldChangeQueue.size() ) {
                    plugin.getServer().getScheduler().cancelTask( worldChangeQueueTaskId );
                    if( is_preview ) {
                        postProcessPreview();
                    } else {
                        postProcess();
                    }
                }
            }
        }, 2L, 2L );
    }

    /**
     * Store the preview session for later use
     */
    public void postProcessPreview() {
        // If there's planned changes, save the preview
        if( is_preview && ( changes_applied_count > 0 || changes_planned_count > 0 ) ) {
            // Append the preview and blocks temporarily
            final PreviewSession ps = new PreviewSession( player, this );
            plugin.playerActivePreviews.put( player.getName(), ps );
            moveEntitiesToSafety();
        }
        fireApplierCallback();
    }

    /**
     * 
     * @return
     */
    public void postProcess() {

        // POST ROLLBACK TRIGGERS
        if( processType.equals( PrismProcessType.ROLLBACK ) ) {

            // We're going to modify the action type of the query params
            // and pass it along to a restore.
            // NOTE: These params have been modified from original, so
            // do NOT use the object for original params.

            // /**
            // * If we've rolled back any containers we need to restore
            // item-removes.
            // */
            // if(parameters.shouldTriggerRollbackFor(ActionType.ITEM_REMOVE)){
            //
            // Prism.debug("Action being rolled back triggers a second rollback: Item Remove");
            //
            // QueryParameters triggerParameters;
            // try {
            // triggerParameters = parameters.clone();
            // triggerParameters.resetActionTypes();
            // triggerParameters.addActionType(ActionType.ITEM_REMOVE);
            //
            // ActionsQuery aq = new ActionsQuery(plugin);
            // QueryResult results = aq.lookup( player, triggerParameters );
            // if(!results.getActionResults().isEmpty()){
            // Rollback rb = new Rollback( plugin, player,
            // results.getActionResults(), triggerParameters );
            // rb.apply();
            // }
            // } catch (CloneNotSupportedException e) {
            // e.printStackTrace();
            // }
            // }
        }

        moveEntitiesToSafety();

        fireApplierCallback();

    }

    /**
	 * 
	 */
    protected void moveEntitiesToSafety() {
        if( parameters.getWorld() != null && player != null ) {
            final List<Entity> entities = player.getNearbyEntities( parameters.getRadius(), parameters.getRadius(),
                    parameters.getRadius() );
            entities.add( player );
            for ( final Entity entity : entities ) {
                if( entity instanceof LivingEntity ) {
                    int add = 0;
                    if( EntityUtils.inCube( parameters.getPlayerLocation(), parameters.getRadius(),
                            entity.getLocation() ) ) {
                        final Location l = entity.getLocation();
                        while ( !EntityUtils.playerMayPassThrough( l.getBlock().getType() ) ) {
                            add++;
                            if( l.getY() >= 256 )
                                break;
                            l.setY( l.getY() + 1 );
                        }
                        if( add > 0 ) {
                            entities_moved.put( entity, add );
                            entity.teleport( l );
                        }
                    }
                }
            }
        }
    }

    /**
	 * 
	 */
    public void fireApplierCallback() {

        // If previewing, the applied count will never apply, we'll
        // assume it's all planned counts
        if( is_preview ) {
            changes_planned_count += changes_applied_count;
            changes_applied_count = 0;
        }

        final ApplierResult results = new ApplierResult( is_preview, changes_applied_count, skipped_block_count,
                changes_planned_count, blockStateChanges, parameters, entities_moved );

        if( callback != null ) {
            callback.handle( sender, results );
        }

        // Trigger the events
        if( processType.equals( PrismProcessType.ROLLBACK ) ) {
            final PrismBlocksRollbackEvent event = new PrismBlocksRollbackEvent( blockStateChanges, player, parameters,
                    results );
            plugin.getServer().getPluginManager().callEvent( event );
        }

        plugin.eventTimer.recordTimedEvent( "applier function complete" );

        // record timed events to log
        if( plugin.getConfig().getBoolean( "prism.debug" ) ) {
            // Flush timed data
            plugin.eventTimer.printTimeRecord();
            Prism.debug( "Changes: " + changes_applied_count );
            Prism.debug( "Planned: " + changes_planned_count );
            Prism.debug( "Skipped: " + skipped_block_count );
        }
    }
}