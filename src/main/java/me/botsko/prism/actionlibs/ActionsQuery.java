package me.botsko.prism.actionlibs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import org.bukkit.Location;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.util.Vector;

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
	 * @param plugin
	 * @return 
	 */
	public ActionsQuery(Prism plugin) {
		this.plugin = plugin;
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
		
		
		// Pull results
		List<Handler> actions = new ArrayList<Handler>();
		
		// Build conditions based off final args
		String query = getArgumentConditions(parameters);
		
		if(query != null){
			try {
				
				Connection conn = Prism.dbc();
				
				plugin.eventTimer.recordTimedEvent("query started");
				
	            PreparedStatement s;
	    		s = conn.prepareStatement(query);
	    		ResultSet rs = s.executeQuery();
	    		
	    		plugin.eventTimer.recordTimedEvent("query returned, building results");
	    		
	    		while(rs.next()){
	    			
	    			if( rs.getString(3) == null ) continue;
	    			
	    			// Get the action handler
	    			ActionType actionType = Prism.getActionRegistry().getAction(rs.getString(3));
	    			
	    			if(actionType == null) continue;
	    			
	    			Handler baseHandler = Prism.getHandlerRegistry().getHandler( actionType.getHandler() );
	   
//	    			plugin.debug("Important: Action type '" + rs.getString(3) + "' has no official handling class, will be shown as generic." );

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
					baseHandler.setBlockSubId( rs.getByte(10) );
					baseHandler.setOldBlockId( rs.getInt(11) );
					baseHandler.setOldBlockSubId( rs.getByte(12) );
					baseHandler.setData( rs.getString(13) );
    				baseHandler.setMaterialAliases( plugin.getItems() );
    				
    				// Set aggregate counts if a lookup
    				int aggregated = 0;
    				if( parameters.getProcessType().equals(PrismProcessType.LOOKUP) && !parameters.hasFlag(Flag.NO_GROUP) ){
    					aggregated = rs.getInt(16);
    				}
    				baseHandler.setAggregateCount(aggregated);
    				
    				actions.add(baseHandler);
	    			
	    		}
	    		
	    		rs.close();
	    		s.close();
	    		conn.close();
	            
	        } catch (SQLException e) {
	            plugin.logDbError( e );
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
		try {
            
			Connection conn = Prism.dbc();
            PreparedStatement s;
    		s = conn.prepareStatement ("SELECT * FROM prism_actions WHERE action_type = 'prism-process' AND player = ? ORDER BY id DESC LIMIT 0,1");
    		s.setString(1, playername);
    		s.executeQuery();
    		ResultSet rs = s.getResultSet();

    		if(rs.first()){
    			id = rs.getInt("id");
			}
    		
    		rs.close();
    		s.close();
    		conn.close();
            
        } catch (SQLException e) {
        	plugin.logDbError( e );
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
		try {
            
			Connection conn = Prism.dbc();
            PreparedStatement s;
    		s = conn.prepareStatement ("SELECT * FROM prism_actions WHERE action_type = 'prism-process' AND id = ?");
    		s.setInt(1, id);
    		s.executeQuery();
    		ResultSet rs = s.getResultSet();

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
    		
    		rs.close();
    		s.close();
    		conn.close();
            
        } catch (SQLException e) {
        	plugin.logDbError( e );
        }
		return process;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public int delete( QueryParameters parameters ){
		int total_rows_affected = 0, cycle_rows_affected;
		try {
			// Build conditions based off final args
			String query = getArgumentConditions( parameters );
			Connection conn = Prism.dbc();
			Statement s = conn.createStatement();
			cycle_rows_affected = s.executeUpdate (query);
			total_rows_affected += cycle_rows_affected;
			s.close();
			conn.close();
		} catch (SQLException e) {
			plugin.logDbError( e );
		}
		return total_rows_affected;
	}
	
	
	/**
	 * 
	 * @param args
	 */
	public String getArgumentConditions( QueryParameters parameters ){
		
		// Build select
		String query = "";
		if( !parameters.getProcessType().equals(PrismProcessType.DELETE) ){
			query += "SELECT " +
					"prism_actions.id, " +
					"prism_actions.action_time, " +
					"prism_actions.action_type, " +
					"prism_actions.player, " +
					"prism_actions.world, " +
					"prism_actions.x, " +
					"prism_actions.y, " +
					"prism_actions.z, " +
					"prism_actions.block_id, " +
					"prism_actions.block_subid, " +
					"prism_actions.old_block_id, " +
					"prism_actions.old_block_subid, " +
					"prism_actions.data, ";
			
			if( plugin.getConfig().getString("prism.database.mode").equalsIgnoreCase("sqlite") ){
				query +=
					"date(prism_actions.action_time) AS display_date, " +
					"time(prism_actions.action_time) AS display_time ";
			}
			else if( plugin.getConfig().getString("prism.database.mode").equalsIgnoreCase("mysql") ){
				query +=
					"DATE_FORMAT(prism_actions.action_time, '%c/%e/%y') AS display_date, " +
					"DATE_FORMAT(prism_actions.action_time, '%l:%i%p') AS display_time";
			}
			
			if( parameters.getProcessType().equals(PrismProcessType.LOOKUP) && !parameters.hasFlag(Flag.NO_GROUP) ){
				query += ", COUNT(id) AS counted";
			}
			
		} else {
			query += "DELETE";
		}
		
		query += " FROM prism_actions WHERE 1=1";
		
		/**
		 * ID
		 * 
		 * If we're querying for an ID, none of the other arguments matter.
		 */
		int id = parameters.getId();
		if(id > 0){
			query += " AND id = " + id;
		} else {
			
			/**
			 * World
			 */
			if( !parameters.allowsNoRadius() && !parameters.getProcessType().equals(PrismProcessType.DELETE) ){
				if( parameters.getWorld() != null ){
					query += " AND world = '"+parameters.getWorld()+"'";
				}
			}

			/**
			 * Actions
			 */
			HashMap<String,MatchRule> action_types = parameters.getActionTypeNames();
			
			// Make sure none of the prism process types are requested
			boolean containtsPrismProcessType = false;
			boolean hasPositiveMatchRule = false;
			if( !action_types.isEmpty() ){
				query += buildMultipleConditions( action_types, "action_type", null );
				for (Entry<String,MatchRule> entry : action_types.entrySet()){
					if(entry.getKey().contains("prism")){
						containtsPrismProcessType = true;
						break;
					}
					if(entry.getValue().equals(MatchRule.INCLUDE)){
						hasPositiveMatchRule = true;
					}
				}
			}
			
			if( !containtsPrismProcessType && !parameters.getProcessType().equals(PrismProcessType.DELETE) && !hasPositiveMatchRule ){
				query += " AND prism_actions.action_type NOT LIKE '%prism%'";
			}
			
			/**
			 * Players
			 */
			HashMap<String,MatchRule> playerNames = parameters.getPlayerNames();
			query += buildMultipleConditions( playerNames, "player", null );
			
			/**
			 * Radius
			 * 
			 * Only build a radius condition if we're doing a select. If doing a delete, 
			 * only use the radius if it was specified by the player.
			 */
			if( !parameters.getProcessType().equals(PrismProcessType.DELETE) || (parameters.getProcessType().equals(PrismProcessType.DELETE) && parameters.getFoundArgs().containsKey("r") ) ){
				query += buildRadiusCondition(parameters.getMinLocation(), parameters.getMaxLocation());
			}

			/**
			 * Block
			 */
			HashMap<Integer,Byte> blockfilters = parameters.getBlockFilters();
			if(!blockfilters.isEmpty()){
				String[] blockArr = new String[blockfilters.size()];
				int i = 0;
				for (Entry<Integer,Byte> entry : blockfilters.entrySet()){
					if( entry.getValue() == 0 ){
						blockArr[i] = "prism_actions.block_id = " + entry.getKey();
					} else {
						blockArr[i] = "prism_actions.block_id = " + entry.getKey() + " AND prism_actions.block_subid = " +  entry.getValue();
					}
					i++;
				}
				query += buildGroupConditions("data", blockArr, "%", "OR", null);
			}
			
			/**
			 * Entity
			 */
			HashMap<String,MatchRule> entityNames = parameters.getEntities();
			query += buildMultipleConditions( entityNames, "data", "entity_name\":\"%s" );
			
			/**
			 * Timeframe
			 */
			String time = parameters.getBeforeTime();
			if(time != null){
				query += buildTimeCondition(time,"<=");
			}
			time = parameters.getSinceTime();
			if(time != null){
				query += buildTimeCondition(time,null);
			}
			
			/**
			 * Keywords
			 */
			String keyword = parameters.getKeyword();
			if(keyword != null){
				query += " AND prism_actions.data LIKE '%"+keyword+"%'";
			}
			
			/**
			 * Specific coords
			 */
			Location loc = parameters.getSpecificBlockLocation();
			if(loc != null){
				query += " AND prism_actions.x = " +(int)loc.getBlockX()+ " AND prism_actions.y = " +(int)loc.getBlockY()+ " AND prism_actions.z = " +(int)loc.getBlockZ();
			}
			
			/**
			 * Parent process id
			 */
			if(parameters.getParentId() > 0){
				query += " AND data = " + parameters.getParentId();
			}
			
			if(!parameters.getProcessType().equals(PrismProcessType.DELETE)){
				
				if( parameters.getProcessType().equals(PrismProcessType.LOOKUP) && !parameters.hasFlag(Flag.NO_GROUP) ){
					query += " GROUP BY prism_actions.action_type, prism_actions.player, prism_actions.block_id, prism_actions.data";
				}
			
				/**
				 * Order by
				 */
				String sort_dir = parameters.getSortDirection();
				query += " ORDER BY prism_actions.action_time "+sort_dir+", x ASC, z ASC, y ASC, id "+sort_dir;
				
				/**
				 * LIMIT
				 */
				int limit = parameters.getLimit();
				if(limit > 0){
					query += " LIMIT "+limit;
				}
			} else {
				int perBatch = plugin.getConfig().getInt("prism.purge.records-per-batch");
				if( perBatch < 100){
					perBatch = 100;
				}
				query += " LIMIT " + perBatch;
			}
		}
		
		query += ";";

		if(plugin.getConfig().getBoolean("prism.debug")){
			plugin.debug(query);
		}
		
		return query;
		
	}
	
	
	/**
	 * 
	 * @param origValues
	 * @param field_name
	 * @return
	 */
	protected String buildMultipleConditions( HashMap<String,MatchRule> origValues, String field_name, String format ){
		String query = "";
		if(!origValues.isEmpty()){
			
			ArrayList<String> whereIs = new ArrayList<String>();
			ArrayList<String> whereNot = new ArrayList<String>();
			ArrayList<String> whereIsLike = new ArrayList<String>();
			for (Entry<String,MatchRule> entry : origValues.entrySet()){
				if(entry.getValue().equals(MatchRule.EXCLUDE)){
					whereNot.add(entry.getKey());
				}
				else if(entry.getValue().equals(MatchRule.PARTIAL)){
					whereIsLike.add(entry.getKey());
				} else {
					whereIs.add(entry.getKey());
				}
			}
			// To match
			if(!whereIs.isEmpty()){
				String[] whereValues = new String[whereIs.size()];
				whereValues = whereIs.toArray(whereValues);
				if(format == null){
					query += buildGroupConditions(field_name, whereValues, "%s = '%s'", "OR", null);
				} else {
					query += buildGroupConditions(field_name, whereValues, "%s LIKE '%%%s%%'", "OR", format);
				}
			}
			// To match partial
			if(!whereIsLike.isEmpty()){
				String[] whereValues = new String[whereIsLike.size()];
				whereValues = whereIsLike.toArray(whereValues);
				query += buildGroupConditions(field_name, whereValues, "%s LIKE '%%%s%%'", "OR", format);
			}
			// Not match
			if(!whereNot.isEmpty()){
				String[] whereNotValues = new String[whereNot.size()];
				whereNotValues = whereNot.toArray(whereNotValues);
				
				if(format == null){
					query += buildGroupConditions(field_name, whereNotValues, "%s != '%s'", null, null);
				} else {
					query += buildGroupConditions(field_name, whereNotValues, "%s NOT LIKE '%%%s%%'", null, format);
				}
			}
		}
		return query;
	}
	
	
	/**
	 * 
	 * @param fieldname
	 * @param arg_values
	 * @return
	 */
	protected String buildGroupConditions( String fieldname, String[] arg_values, String matchFormat, String matchType, String dataFormat ){
		
		String where = "";
		matchFormat = (matchFormat == null ? "%s = %s" : matchFormat);
		matchType = (matchType == null ? "AND" : matchType);
		dataFormat = (dataFormat == null ? "%s" : dataFormat);
		
		if( arg_values.length > 0 && !matchFormat.isEmpty() ){
			where += " AND (";
			int c = 1;
			for(String val : arg_values){
				if(c > 1 && c <= arg_values.length){
					where += " "+matchType+" ";
				}
				where += String.format(matchFormat, "prism_actions."+fieldname, String.format(dataFormat,val));
				c++;
			}
			where += ")";
		}
		return where;
	}
	
	
	/**
	 * 
	 * @param arg_values
	 * @param player_name
	 * @return
	 */
	protected String buildRadiusCondition( Vector minLoc, Vector maxLoc ){
		String where = "";
		if(minLoc != null && maxLoc != null ){
			where += " AND (prism_actions.x BETWEEN " + minLoc.getX() + " AND " + maxLoc.getX() + ")";
			where += " AND (prism_actions.y BETWEEN " + minLoc.getY() + " AND " + maxLoc.getY() + ")";
			where += " AND (prism_actions.z BETWEEN " + minLoc.getZ() + " AND " + maxLoc.getZ() + ")";
		}
		return where;
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String buildTimeCondition( String dateFrom, String equation ){
		String where = "";
		if(dateFrom != null){
			if(equation == null){
				where += " AND prism_actions.action_time >= '" + dateFrom + "'";
			} else {
				where += " AND prism_actions.action_time "+equation+" '" + dateFrom + "'";
			}
		}
		return where;
	}
}