package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.PrismApplierCallback;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.appliers.Restore;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.entity.Player;

import java.util.ArrayList;
import java.util.List;

public class RestoreCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     * @return
     */
    public RestoreCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(final CallInfo call) {

        final QueryParameters parameters = PreprocessArgs.process( plugin, call.getSender(), call.getArgs(),
                PrismProcessType.RESTORE, 1, !plugin.getConfig().getBoolean( "prism.queries.never-use-defaults" ) );
        if( parameters == null ) { return; }
        parameters.setProcessType( PrismProcessType.RESTORE );
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

        call.getSender().sendMessage(
                Prism.messenger.playerSubduedHeaderMsg( "Preparing results..." + defaultsReminder ) );

        /**
         * Run the query itself in an async task so the lookup query isn't done
         * on the main thread
         */
        plugin.getServer().getScheduler().runTaskAsynchronously( plugin, new Runnable() {
            @Override
            public void run() {

                final ActionsQuery aq = new ActionsQuery( plugin );
                final QueryResult results = aq.lookup( parameters, call.getSender() );
                if( !results.getActionResults().isEmpty() ) {

                    call.getSender().sendMessage( Prism.messenger.playerHeaderMsg( "Restoring changes..." ) );

                    // Inform nearby players
                    if( call.getSender() instanceof Player ) {
                        final Player player = (Player) call.getSender();
                        plugin.notifyNearby( player, parameters.getRadius(), player.getDisplayName()
                                + " is re-applying block changes nearby. Just so you know." );
                    }

                    // Perform restore on the main thread
                    plugin.getServer().getScheduler().runTask( plugin, new Runnable() {
                        @Override
                        public void run() {
                            final Restore rs = new Restore( plugin, call.getSender(), results.getActionResults(),
                                    parameters, new PrismApplierCallback() );
                            rs.apply();
                        }
                    } );

                } else {
                    call.getSender()
                            .sendMessage(
                                    Prism.messenger
                                            .playerError( "Nothing found to restore. Try using /prism l (args) first." ) );
                }
            }
        } );
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return PreprocessArgs.complete( call.getSender(), call.getArgs() );
    }
}