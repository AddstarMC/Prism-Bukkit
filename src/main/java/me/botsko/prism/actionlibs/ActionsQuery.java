package me.botsko.prism.actionlibs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.actions.PrismProcessAction;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;

public class ActionsQuery {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private QueryBuilder qb;
	
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
		this.qb = new QueryBuilder(plugin);
	}
	
	
	/**
	 * 
	 * @return
	 */
	public QueryResult lookup( QueryParameters parameters ){
		return lookup( parameters, null );
	}
	
	
	/**
	 * 
	 * @return
	 */
	public QueryResult lookup( QueryParameters parameters, CommandSender sender ){
		
		Player player = null;
		if(sender instanceof Player){
			player = (Player) sender;
		}
		
		// If lookup, determine if we need to group
		shouldGroup = false;
		if( parameters.getProcessType().equals(PrismProcessType.LOOKUP)){
			shouldGroup = true;
			// What to default to
			if( !plugin.getConfig().getBoolean("prism.queries.lookup-auto-group") ){
				shouldGroup = false;
			}
			// Any overriding flags passed?
			if( parameters.hasFlag(Flag.NO_GROUP) || parameters.hasFlag(Flag.EXTENDED) ){
				shouldGroup = false;
			}
		}
		
		
		// Pull results
		List<Handler> actions = new ArrayList<Handler>();
		
		// Build conditions based off final args
		String query = qb.buildQuery(parameters, shouldGroup);
		
		if(query != null){
			Connection conn = null;
			PreparedStatement s = null;
			ResultSet rs = null;
			try {
				
				plugin.eventTimer.recordTimedEvent("query started");
				
				conn = Prism.dbc();
	    		s = conn.prepareStatement(query);
	    		rs = s.executeQuery();
	    		
	    		plugin.eventTimer.recordTimedEvent("query returned, building results");
	    		
	    		while(rs.next()){
	    			
	    			if( rs.getString(3) == null ) continue;
	    			
	    			// Get the action handler
	    			ActionType actionType = Prism.getActionRegistry().getAction(rs.getString(3));
	    			
	    			if(actionType == null) continue;
	    			
	    			Handler baseHandler = Prism.getHandlerRegistry().getHandler( actionType.getHandler() );
	   
//	    			Prism.debug("Important: Action type '" + rs.getString(3) + "' has no official handling class, will be shown as generic." );

    				// Set all shared values
	    			baseHandler.setPlugin( plugin );
	    			baseHandler.setType( actionType );
	    			baseHandler.setId( rs.getInt(1) );
	    			baseHandler.setActionTime( rs.getString(2) );
	    			baseHandler.setPlayerName( rs.getString(4) );
	    			baseHandler.setWorldName( rs.getString(5) );
	    			baseHandler.setX( rs.getInt(6) );
	    			baseHandler.setY( rs.getInt(7) );
	    			baseHandler.setZ( rs.getInt(8) );
	    			baseHandler.setDisplayDate( rs.getString(14) );
	    			baseHandler.setDisplayTime( rs.getString(15) );
					baseHandler.setBlockId( rs.getInt(9) );
					baseHandler.setBlockSubId( rs.getInt(10) );
					baseHandler.setOldBlockId( rs.getInt(11) );
					baseHandler.setOldBlockSubId( rs.getInt(12) );
					baseHandler.setData( rs.getString(13) );
    				baseHandler.setMaterialAliases( plugin.getItems() );
    				
    				// Set aggregate counts if a lookup
    				int aggregated = 0;
    				if( shouldGroup ){
    					aggregated = rs.getInt(16);
    				}
    				baseHandler.setAggregateCount(aggregated);
    				
    				actions.add(baseHandler);
	    			
	    		}
	            
	        } catch (SQLException e) {
	        	plugin.handleDatabaseException( e );
	        } finally {
	        	if(rs != null) try { rs.close(); } catch (SQLException e) {}
	        	if(s != null) try { s.close(); } catch (SQLException e) {}
	        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
	        }
		}
		
		// Build result object
		QueryResult res = new QueryResult( actions, parameters );
		res.setPerPage( parameters.getPerPage() );
		
		// Cache it if we're doing a lookup. Otherwise we don't
		// need a cache.
		if(parameters.getProcessType().equals(PrismProcessType.LOOKUP)){
			String keyName = "console";
			if( player != null ){
				keyName = player.getName();
			}
			if(plugin.cachedQueries.containsKey(keyName)){
				plugin.cachedQueries.remove(keyName);
			}
			plugin.cachedQueries.put(keyName, res);
			// We also need to share these results with the -share-with players.
			for(String sharedPlayer : parameters.getSharedPlayers()){
				plugin.cachedQueries.put(sharedPlayer, res);
			}
		}
		
		plugin.eventTimer.recordTimedEvent("results object completed");
		
		// Return it
		return res;
		
	}
	
	
	/**
	 * 
	 * @param person
	 * @param account_name
	 */
	public int getUsersLastPrismProcessId( String playername ){
		int id = 0;
		Connection conn = null;
		PreparedStatement s = null;
		ResultSet rs = null;
		try {
            
			conn = Prism.dbc();
    		s = conn.prepareStatement ("SELECT * FROM prism_actions WHERE action_type = 'prism-process' AND player = ? ORDER BY id DESC LIMIT 0,1");
    		s.setString(1, playername);
    		s.executeQuery();
    		rs = s.getResultSet();

    		if(rs.first()){
    			id = rs.getInt("id");
			}
            
        } catch (SQLException e) {
        	plugin.handleDatabaseException( e );
        } finally {
        	if(rs != null) try { rs.close(); } catch (SQLException e) {}
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
		return id;
	}
	
	
	/**
	 * 
	 * @param person
	 * @param account_name
	 */
	public PrismProcessAction getPrismProcessRecord( int id ){
		PrismProcessAction process = null;
		Connection conn = null;
		PreparedStatement s = null;
		ResultSet rs = null;
		try {
            
			conn = Prism.dbc();
    		s = conn.prepareStatement ("SELECT * FROM prism_actions WHERE action_type = 'prism-process' AND id = ?");
    		s.setInt(1, id);
    		s.executeQuery();
    		rs = s.getResultSet();

    		if(rs.first()){
    			process = new PrismProcessAction();
    			// Set all shared values
    			process.setId( rs.getInt("id") );
    			process.setType( Prism.getActionRegistry().getAction( rs.getString("action_type") ) );
    			process.setActionTime( rs.getString("action_time") );
    			process.setWorldName( rs.getString("world") );
    			process.setPlayerName( rs.getString("player") );
    			process.setX( rs.getInt("x") );
    			process.setY( rs.getInt("y") );
    			process.setZ( rs.getInt("z") );
    			process.setData( rs.getString("data") );
			}
            
        } catch (SQLException e) {
        	plugin.handleDatabaseException( e );
        } finally {
        	if(rs != null) try { rs.close(); } catch (SQLException e) {}
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
		return process;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public int delete( QueryParameters parameters ){
		int total_rows_affected = 0, cycle_rows_affected;
		Connection conn = null;
		Statement s = null;
		try {
			// Build conditions based off final args
			String query = qb.buildQuery(parameters, shouldGroup);
			conn = Prism.dbc();
			s = conn.createStatement();
			cycle_rows_affected = s.executeUpdate (query);
			total_rows_affected += cycle_rows_affected;
		} catch (SQLException e) {
			plugin.handleDatabaseException( e );
		} finally {
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
		return total_rows_affected;
	}
}