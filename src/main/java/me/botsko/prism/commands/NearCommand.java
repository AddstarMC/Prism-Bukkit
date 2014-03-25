package me.botsko.prism.commands;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.commandlibs.SubHandler;

import java.util.List;

public class NearCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     * @return
     */
    public NearCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(final CallInfo call) {

        // Build params
        final QueryParameters parameters = new QueryParameters();
        parameters.setPerPage( plugin.getConfig().getInt( "prism.queries.default-results-per-page" ) );
        parameters.setWorld( call.getPlayer().getWorld().getName() );

        // allow a custom near radius
        int radius = plugin.getConfig().getInt( "prism.near.default-radius" );
        if( call.getArgs().length == 2 ) {
            if( TypeUtils.isNumeric( call.getArg( 1 ) ) ) {
                final int _tmp_radius = Integer.parseInt( call.getArg( 1 ) );
                if( _tmp_radius > 0 ) {
                    radius = _tmp_radius;
                } else {
                    call.getPlayer()
                            .sendMessage(
                                    Prism.messenger
                                            .playerError( "Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help." ) );
                    return;
                }
            } else {
                call.getPlayer()
                        .sendMessage(
                                Prism.messenger
                                        .playerError( "Radius must be a number. Or leave it off to use the default. Use /prism ? for help." ) );
                return;
            }
        }

        parameters.setRadius( radius );
        parameters.setMinMaxVectorsFromPlayerLocation( call.getPlayer().getLocation() );
        parameters.setLimit( plugin.getConfig().getInt( "prism.near.max-results" ) );

        /**
         * Run the lookup itself in an async task so the lookup query isn't done
         * on the main thread
         */
        plugin.getServer().getScheduler().runTaskAsynchronously( plugin, new Runnable() {
            @Override
            public void run() {

                final ActionsQuery aq = new ActionsQuery( plugin );
                final QueryResult results = aq.lookup( parameters, call.getPlayer() );
                if( !results.getActionResults().isEmpty() ) {
                    call.getPlayer().sendMessage(
                            Prism.messenger.playerSubduedHeaderMsg( "All changes within " + parameters.getRadius()
                                    + " blocks of you..." ) );
                    call.getPlayer().sendMessage(
                            Prism.messenger.playerHeaderMsg( "Showing " + results.getTotalResults()
                                    + " results. Page 1 of " + results.getTotal_pages() ) );
                    final List<Handler> paginated = results.getPaginatedActionResults();
                    if( paginated != null ) {
                        int result_count = results.getIndexOfFirstResult();
                        for ( final Handler a : paginated ) {
                            final ActionMessage am = new ActionMessage( a );
                            if( parameters.allowsNoRadius() || parameters.hasFlag( Flag.EXTENDED )
                                    || plugin.getConfig().getBoolean( "prism.messenger.always-show-extended" ) ) {
                                am.showExtended();
                            }
                            am.setResultIndex( result_count );
                            call.getPlayer().sendMessage( Prism.messenger.playerMsg( am.getMessage() ) );
                            result_count++;
                        }

                        // Flush timed data
                        plugin.eventTimer.printTimeRecord();

                    } else {
                        call.getPlayer()
                                .sendMessage(
                                        Prism.messenger
                                                .playerError( "Pagination can't find anything. Do you have the right page number?" ) );
                    }
                } else {
                    call.getPlayer().sendMessage( Prism.messenger.playerError( "Couldn't find anything." ) );
                }
            }
        } );
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}