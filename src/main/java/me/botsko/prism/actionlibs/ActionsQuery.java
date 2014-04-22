package me.botsko.prism.actionlibs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.actions.PrismProcessAction;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.database.mysql.DeleteQueryBuilder;
import me.botsko.prism.database.mysql.SelectQueryBuilder;

public class ActionsQuery {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
	 * 
	 */
    private final SelectQueryBuilder qb;

    /**
	 * 
	 */
    private boolean shouldGroup = false;

    /**
     * 
     * @param plugin
     * @return
     */
    public ActionsQuery(Prism plugin) {
        this.plugin = plugin;
        this.qb = new SelectQueryBuilder( plugin );
    }

    /**
     * 
     * @return
     */
    public QueryResult lookup(QueryParameters parameters) {
        return lookup( parameters, null );
    }

    /**
     * 
     * @return
     */
    public QueryResult lookup(QueryParameters parameters, CommandSender sender) {

        Player player = null;
        if( sender instanceof Player ) {
            player = (Player) sender;
        }

        // If lookup, determine if we need to group
        shouldGroup = false;
        if( parameters.getProcessType().equals( PrismProcessType.LOOKUP ) ) {
            shouldGroup = true;
            // What to default to
            if( !plugin.getConfig().getBoolean( "prism.queries.lookup-auto-group" ) ) {
                shouldGroup = false;
            }
            // Any overriding flags passed?
            if( parameters.hasFlag( Flag.NO_GROUP ) || parameters.hasFlag( Flag.EXTENDED ) ) {
                shouldGroup = false;
            }
        }

        // Pull results
        final List<Handler> actions = new ArrayList<Handler>();

        // Build conditions based off final args
        final String query = qb.getQuery( parameters, shouldGroup );

        if( query != null ) {
            Connection conn = null;
            PreparedStatement s = null;
            ResultSet rs = null;
            try {

                plugin.eventTimer.recordTimedEvent( "query started" );

                conn = Prism.dbc();

                // Handle dead connections
                if( conn == null || conn.isClosed() ) {
                    if( RecordingManager.failedDbConnectionCount == 0 ) {
                        Prism.log( "Prism database error. Connection should be there but it's not. Leaving actions to log in queue." );
                    }
                    RecordingManager.failedDbConnectionCount++;
                    sender.sendMessage( Prism.messenger
                            .playerError( "Database connection was closed, please wait and try again." ) );
                    return new QueryResult( actions, parameters );
                } else {
                    RecordingManager.failedDbConnectionCount = 0;
                }

                s = conn.prepareStatement( query );
                rs = s.executeQuery();

                plugin.eventTimer.recordTimedEvent( "query returned, building results" );

                while ( rs.next() ) {

                    if( rs.getString( 3 ) == null )
                        continue;

                    // Convert action ID to name
                    // Performance-wise this is a lot faster than table joins
                    // and the cache data should always be available
                    String actionName = "";
                    for ( final Entry<String, Integer> entry : Prism.prismActions.entrySet() ) {
                        if( entry.getValue() == rs.getInt( 3 ) ) {
                            actionName = entry.getKey();
                        }
                    }
                    if( actionName.isEmpty() ) {
                        Prism.log( "Record contains action ID that doesn't exist in cache: " + rs.getInt( 3 ) );
                        continue;
                    }

                    // Get the action handler
                    final ActionType actionType = Prism.getActionRegistry().getAction( actionName );

                    if( actionType == null )
                        continue;

                    // Prism.debug("Important: Action type '" + rs.getString(3)
                    // +
                    // "' has no official handling class, will be shown as generic."
                    // );

                    try {

                        final Handler baseHandler = Prism.getHandlerRegistry().getHandler( actionType.getHandler() );

                        // Convert world ID to name
                        // Performance-wise this is typically a lot faster than
                        // table joins
                        String worldName = "";
                        for ( final Entry<String, Integer> entry : Prism.prismWorlds.entrySet() ) {
                            if( entry.getValue() == rs.getInt( 5 ) ) {
                                worldName = entry.getKey();
                            }
                        }

                        // Set all shared values
                        baseHandler.setPlugin( plugin );
                        baseHandler.setType( actionType );
                        baseHandler.setId( rs.getInt( 1 ) );
                        baseHandler.setUnixEpoch( rs.getString( 2 ) );
                        baseHandler.setPlayerName( rs.getString( 4 ) );
                        baseHandler.setWorldName( worldName );
                        baseHandler.setX( rs.getInt( 6 ) );
                        baseHandler.setY( rs.getInt( 7 ) );
                        baseHandler.setZ( rs.getInt( 8 ) );
                        baseHandler.setBlockId( rs.getInt( 9 ) );
                        baseHandler.setBlockSubId( rs.getInt( 10 ) );
                        baseHandler.setOldBlockId( rs.getInt( 11 ) );
                        baseHandler.setOldBlockSubId( rs.getInt( 12 ) );
                        baseHandler.setData( rs.getString( 13 ) );
                        baseHandler.setMaterialAliases( Prism.getItems() );

                        // Set aggregate counts if a lookup
                        int aggregated = 0;
                        if( shouldGroup ) {
                            aggregated = rs.getInt( 14 );
                        }
                        baseHandler.setAggregateCount( aggregated );

                        actions.add( baseHandler );

                    } catch ( final Exception e ) {
                        if( !rs.isClosed() ) {
                            Prism.log( "Ignoring data from record #" + rs.getInt( 1 ) + " because it caused an error:" );
                        }
                        e.printStackTrace();
                    }
                }
            } catch ( final SQLException e ) {
                plugin.handleDatabaseException( e );
            } finally {
                if( rs != null )
                    try {
                        rs.close();
                    } catch ( final SQLException ignored ) {}
                if( s != null )
                    try {
                        s.close();
                    } catch ( final SQLException ignored ) {}
                if( conn != null )
                    try {
                        conn.close();
                    } catch ( final SQLException ignored ) {}
            }
        }

        // Build result object
        final QueryResult res = new QueryResult( actions, parameters );
        res.setPerPage( parameters.getPerPage() );

        // Cache it if we're doing a lookup. Otherwise we don't
        // need a cache.
        if( parameters.getProcessType().equals( PrismProcessType.LOOKUP ) ) {
            String keyName = "console";
            if( player != null ) {
                keyName = player.getName();
            }
            if( plugin.cachedQueries.containsKey( keyName ) ) {
                plugin.cachedQueries.remove( keyName );
            }
            plugin.cachedQueries.put( keyName, res );
            // We also need to share these results with the -share-with players.
            for ( final CommandSender sharedPlayer : parameters.getSharedPlayers() ) {
                plugin.cachedQueries.put( sharedPlayer.getName(), res );
            }
        }

        plugin.eventTimer.recordTimedEvent( "results object completed" );

        // Return it
        return res;

    }

    /**
     * 
     * @param playername
     */
    public int getUsersLastPrismProcessId(String playername) {
        String prefix = plugin.getConfig().getString("prism.mysql.prefix");
        int id = 0;
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            final int action_id = Prism.prismActions.get( "prism-process" );

            conn = Prism.dbc();

            if( conn != null && !conn.isClosed() ) {
                s = conn.prepareStatement( "SELECT id FROM " + prefix + "data JOIN " + prefix + "players p ON p.player_id = " + prefix + "data.player_id WHERE action_id = ? AND p.player = ? ORDER BY id DESC LIMIT 1" );
                s.setInt( 1, action_id );
                s.setString( 2, playername );
                s.executeQuery();
                rs = s.getResultSet();

                if( rs.first() ) {
                    id = rs.getInt( "id" );
                }
            } else {
                Prism.log( "Prism database error. getUsersLastPrismProcessId cannot continue." );
            }
        } catch ( final SQLException e ) {
            plugin.handleDatabaseException( e );
        } finally {
            if( rs != null )
                try {
                    rs.close();
                } catch ( final SQLException ignored ) {}
            if( s != null )
                try {
                    s.close();
                } catch ( final SQLException ignored ) {}
            if( conn != null )
                try {
                    conn.close();
                } catch ( final SQLException ignored ) {}
        }
        return id;
    }

    /**
     * 
     * @param id
     */
    public PrismProcessAction getPrismProcessRecord(int id) {
        String prefix = plugin.getConfig().getString("prism.mysql.prefix");
        PrismProcessAction process = null;
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            String sql = "SELECT id, action, epoch, world, player, x, y, z, data FROM " + prefix + "data d";
            // Joins
            sql += " INNER JOIN " + prefix + "players p ON p.player_id = d.player_id ";
            sql += " INNER JOIN " + prefix + "actions a ON a.action_id = d.action_id ";
            sql += " INNER JOIN " + prefix + "worlds w ON w.world_id = d.world_id ";
            sql += " LEFT JOIN " + prefix + "data_extra ex ON ex.data_id = d.id ";
            sql += " WHERE d.id = ?";

            conn = Prism.dbc();

            if( conn != null && !conn.isClosed() ) {
                s = conn.prepareStatement( sql );
                s.setInt( 1, id );
                s.executeQuery();
                rs = s.getResultSet();

                if( rs.first() ) {
                    process = new PrismProcessAction();
                    // Set all shared values
                    process.setId( rs.getInt( "id" ) );
                    process.setType( Prism.getActionRegistry().getAction( rs.getString( "action" ) ) );
                    process.setUnixEpoch( rs.getString( "epoch" ) );
                    process.setWorldName( rs.getString( "world" ) );
                    process.setPlayerName( rs.getString( "player" ) );
                    process.setX( rs.getInt( "x" ) );
                    process.setY( rs.getInt( "y" ) );
                    process.setZ( rs.getInt( "z" ) );
                    process.setData( rs.getString( "data" ) );
                }
            } else {
                Prism.log( "Prism database error. getPrismProcessRecord cannot continue." );
            }
        } catch ( final SQLException e ) {
            plugin.handleDatabaseException( e );
        } finally {
            if( rs != null )
                try {
                    rs.close();
                } catch ( final SQLException ignored ) {}
            if( s != null )
                try {
                    s.close();
                } catch ( final SQLException ignored ) {}
            if( conn != null )
                try {
                    conn.close();
                } catch ( final SQLException ignored ) {}
        }
        return process;
    }

    /**
     * 
     * @return
     */
    public int delete(QueryParameters parameters) {
        int total_rows_affected = 0, cycle_rows_affected;
        Connection conn = null;
        Statement s = null;
        try {
            final DeleteQueryBuilder dqb = new DeleteQueryBuilder( plugin );
            // Build conditions based off final args
            final String query = dqb.getQuery( parameters, shouldGroup );
            conn = Prism.dbc();
            if( conn != null && !conn.isClosed() ) {
                s = conn.createStatement();
                cycle_rows_affected = s.executeUpdate( query );
                total_rows_affected += cycle_rows_affected;
            } else {
                Prism.log( "Prism database error. Purge cannot continue." );
            }
        } catch ( final SQLException e ) {
            plugin.handleDatabaseException( e );
        } finally {
            if( s != null )
                try {
                    s.close();
                } catch ( final SQLException ignored ) {}
            if( conn != null )
                try {
                    conn.close();
                } catch ( final SQLException ignored ) {}
        }
        return total_rows_affected;
    }
}