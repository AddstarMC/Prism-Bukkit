package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.purge.PurgeChunkingUtil;
import me.botsko.prism.purge.PurgeTask;
import me.botsko.prism.purge.SenderPurgeCallback;
import org.bukkit.ChatColor;
import org.bukkit.scheduler.BukkitTask;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class DeleteCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
	 * 
	 */
    protected BukkitTask deleteTask;

    /**
	 * 
	 */
    protected int total_records_affected = 0, cycle_rows_affected = 0;

    /**
     * 
     * @param plugin
     * @return
     */
    public DeleteCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(final CallInfo call) {

        // Allow for canceling tasks
        if( call.getArgs().length > 1 && call.getArg( 1 ).equals( "cancel" ) ) {
            if( plugin.getPurgeManager().deleteTask != null ) {
                plugin.getPurgeManager().deleteTask.cancel();
                call.getSender().sendMessage( Prism.messenger.playerMsg( "Current purge tasks have been canceled." ) );
            } else {
                call.getSender().sendMessage( Prism.messenger.playerError( "No purge task is currently running." ) );
            }
            return;
        }

        // Allow for wiping live queue
        if( call.getArgs().length > 1 && call.getArg( 1 ).equals( "queue" ) ) {
            if( RecordingQueue.getQueue().size() > 0 ) {
                Prism.log( "User " + call.getSender().getName()
                        + " wiped the live queue before it could be written to the database. "
                        + RecordingQueue.getQueue().size() + " events lost." );
                RecordingQueue.getQueue().clear();
                call.getSender().sendMessage( Prism.messenger.playerSuccess( "Unwritten data in queue cleared." ) );
            } else {
                call.getSender().sendMessage( Prism.messenger.playerError( "Event queue is empty, nothing to wipe." ) );
            }
            return;
        }

        // Process and validate all of the arguments
        final QueryParameters parameters = PreprocessArgs.process( plugin, call.getSender(), call.getArgs(),
                PrismProcessType.DELETE, 1, !plugin.getConfig().getBoolean( "prism.queries.never-use-defaults" ) );
        if( parameters == null ) { return; }
        parameters.setStringFromRawArgs( call.getArgs(), 1 );

        // determine if defaults were used
        final ArrayList<String> defaultsUsed = parameters.getDefaultsUsed();
        String defaultsReminder = "";
        if( !defaultsUsed.isEmpty() ) {
            defaultsReminder += " using defaults:";
            for ( final String d : defaultsUsed ) {
                defaultsReminder += " " + d;
            }
        }

        if( parameters.getFoundArgs().size() > 0 ) {

            // Identify the minimum for chunking
            final int minId = PurgeChunkingUtil.getMinimumPrimaryKey();
            if( minId == 0 ) {
                call.getSender().sendMessage(
                        Prism.messenger.playerError( "No minimum primary key could be found for purge chunking" ) );
                return;
            }

            // Identify the max id for chunking
            final int maxId = PurgeChunkingUtil.getMaximumPrimaryKey();
            if( maxId == 0 ) {
                call.getSender().sendMessage(
                        Prism.messenger.playerError( "No maximum primary key could be found for purge chunking" ) );
                return;
            }

            call.getSender()
                    .sendMessage( Prism.messenger.playerSubduedHeaderMsg( "Purging data..." + defaultsReminder ) );

            int purge_tick_delay = plugin.getConfig().getInt( "prism.purge.batch-tick-delay" );
            if( purge_tick_delay < 1 ) {
                purge_tick_delay = 20;
            }

            call.getSender().sendMessage(
                    Prism.messenger.playerHeaderMsg( "Starting purge cycle." + ChatColor.GRAY
                            + " No one will ever know..." ) );

            // build callback
            final SenderPurgeCallback callback = new SenderPurgeCallback();
            callback.setSender( call.getSender() );

            // add to an arraylist so we're consistent
            final CopyOnWriteArrayList<QueryParameters> paramList = new CopyOnWriteArrayList<QueryParameters>();
            paramList.add( parameters );

            Prism.log( "Beginning prism database purge cycle. Will be performed in batches so we don't tie up the db..." );
            deleteTask = plugin
                    .getServer()
                    .getScheduler()
                    .runTaskAsynchronously( plugin,
                            new PurgeTask( plugin, paramList, purge_tick_delay, minId, maxId, callback ) );

        } else {
            call.getSender().sendMessage( Prism.messenger.playerError( "You must supply at least one parameter." ) );
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return PreprocessArgs.complete( call.getSender(), call.getArgs() );
    }
}