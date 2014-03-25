package me.botsko.prism.commands;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Player;

import java.util.List;

public class TeleportCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     * @return
     */
    public TeleportCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(CallInfo call) {

        // Is there anything even stored to paginate?
        String keyName = "console";
        if( call.getSender() instanceof Player ) {
            keyName = call.getSender().getName();
        }
        if( !plugin.cachedQueries.containsKey( keyName ) && !call.getArg( 1 ).contains( "id:" ) ) {
            call.getSender()
                    .sendMessage(
                            Prism.messenger
                                    .playerError( "There's no saved query to use results from. Maybe they expired? Try your lookup again." ) );
            return;
        }

        // Parse the incoming ident
        String ident = call.getArg( 1 );
        if( ident.contains( "id:" ) ) {
            ident = ident.replace( "id:", "" );
        }

        // Determine result index to tp to - either an id, or the next/previous
        // id
        int record_id;
        if( ident.equals( "next" ) || ident.equals( "prev" ) ) {
            // Get stored results
            final QueryResult results = plugin.cachedQueries.get( keyName );
            record_id = results.getLastTeleportIndex();
            record_id = ( record_id == 0 ? 1 : record_id );
            if( record_id > 0 ) {
                if( ident.equals( "next" ) ) {
                    record_id++;
                } else {
                    if( record_id > 1 ) {
                        record_id--;
                    }
                }
            }
        } else {
            if( !TypeUtils.isNumeric( ident ) ) {
                call.getPlayer()
                        .sendMessage(
                                Prism.messenger
                                        .playerError( "You must provide a numeric result number or record ID to teleport to." ) );
                return;
            }
            record_id = Integer.parseInt( ident );
            if( record_id <= 0 ) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerError( "Result number or record ID must be greater than zero." ) );
                return;
            }
        }

        // If a record id provided, re-query the database
        Handler destinationAction;
        if( call.getArg( 1 ).contains( "id:" ) ) {

            // Build params
            final QueryParameters params = new QueryParameters();
            params.setWorld( call.getPlayer().getWorld().getName() );
            params.setId( record_id );

            // Query
            final ActionsQuery aq = new ActionsQuery( plugin );
            final QueryResult results = aq.lookup( params, call.getPlayer() );
            if( results.getActionResults().isEmpty() ) {
                call.getPlayer().sendMessage( Prism.messenger.playerError( "No records exists with this ID." ) );
                return;
            }

            // Get the first result
            destinationAction = results.getActionResults().get( 0 );

        }
        // Otherwise, look for a cached query
        else {

            // Get stored results
            final QueryResult results = plugin.cachedQueries.get( keyName );

            if( record_id > results.getActionResults().size() ) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerError( "No records exists at this index. Did you mean /pr tp id:"
                                + record_id + " instead?" ) );
                return;
            }

            final int key = ( record_id - 1 );

            // Get the result index specified
            destinationAction = results.getActionResults().get( key );

            // Refresh the query time and replace
            results.setQueryTime();
            results.setLastTeleportIndex( record_id );
            plugin.cachedQueries.replace( keyName, results );

        }

        if( destinationAction != null ) {
            final World world = plugin.getServer().getWorld( destinationAction.getWorldName() );
            if( world == null ) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerError( "Action record occurred in world we can't find anymore." ) );
                return;
            }
            final Location loc = new Location( world, destinationAction.getX(), destinationAction.getY(),
                    destinationAction.getZ() );
            call.getPlayer().teleport( loc );
            call.getPlayer().sendMessage(
                    Prism.messenger.playerSubduedHeaderMsg( "Teleporting... " + ChatColor.WHITE
                            + destinationAction.getType().getName() + ChatColor.GRAY + " by " + ChatColor.WHITE
                            + destinationAction.getPlayerName() + ChatColor.GRAY + ", " + ChatColor.WHITE
                            + destinationAction.getTimeSince() ) );
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}