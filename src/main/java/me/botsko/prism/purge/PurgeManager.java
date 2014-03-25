package me.botsko.prism.purge;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.bukkit.Bukkit;
import org.bukkit.scheduler.BukkitTask;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.PreprocessArgs;

final public class PurgeManager implements Runnable {

    private final List<String> purgeRules;
    private final Prism plugin;
    public BukkitTask deleteTask;

    /**
     * 
     * @param purgeRules
     */
    public PurgeManager(Prism plugin, List<String> purgeRules) {
        this.plugin = plugin;
        this.purgeRules = purgeRules;
    }

    /**
	 * 
	 */
    @Override
    public void run() {

        Prism.log( "Scheduled purge executor beginning new run..." );

        if( !purgeRules.isEmpty() ) {

            final CopyOnWriteArrayList<QueryParameters> paramList = new CopyOnWriteArrayList<QueryParameters>();

            for ( final String purgeArgs : purgeRules ) {

                // Process and validate all of the arguments
                final QueryParameters parameters = PreprocessArgs.process( plugin, null, purgeArgs.split( " " ),
                        PrismProcessType.DELETE, 0, false );

                if( parameters == null ) {
                    Prism.log( "Invalid parameters for database purge: " + purgeArgs );
                    continue;
                }

                if( parameters.getFoundArgs().size() > 0 ) {
                    parameters.setStringFromRawArgs( purgeArgs.split( " " ), 0 );
                    paramList.add( parameters );
                }
            }

            if( paramList.size() > 0 ) {

                // Identify the minimum for chunking
                final int minId = PurgeChunkingUtil.getMinimumPrimaryKey();
                if( minId == 0 ) {
                    Prism.log( "No minimum primary key could be found for purge chunking." );
                    return;
                }

                // Identify the max id for chunking
                final int maxId = PurgeChunkingUtil.getMaximumPrimaryKey();
                if( maxId == 0 ) {
                    Prism.log( "No maximum primary key could be found for purge chunking." );
                    return;
                }

                int purge_tick_delay = plugin.getConfig().getInt( "prism.purge.batch-tick-delay" );
                if( purge_tick_delay < 1 ) {
                    purge_tick_delay = 20;
                }

                /**
                 * We're going to cycle through the param rules, one rule at a
                 * time in a single async task. This task will reschedule itself
                 * when each purge cycle has completed and records remain
                 */
                Prism.log( "Beginning prism database purge cycle. Will be performed in batches so we don't tie up the db..." );
                deleteTask = Bukkit
                        .getServer()
                        .getScheduler()
                        .runTaskLaterAsynchronously(
                                plugin,
                                new PurgeTask( plugin, paramList, purge_tick_delay, minId, maxId,
                                        new LogPurgeCallback() ), purge_tick_delay );

            }
        } else {
            Prism.log( "Purge rules are empty, not purging anything." );
        }
    }
}