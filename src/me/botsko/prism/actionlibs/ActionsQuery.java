package me.botsko.prism.actionlibs;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.bukkit.Location;
import org.bukkit.entity.Player;
import org.bukkit.util.Vector;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.GenericAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.PlayerDeathAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.utils.TypeUtils;

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
	public QueryResult lookup( Player player, QueryParameters parameters ){
		
		// Pull results
		List<Action> actions = new ArrayList<Action>();
		
		// Build conditions based off final args
		String query = getArgumentConditions(parameters);
		
		if(query!= null){
			try {
	            
				plugin.dbc();
				
	            PreparedStatement s;
	    		s = plugin.conn.prepareStatement(query);
	    		s.executeQuery();
	    		ResultSet rs = s.getResultSet();
	    		
	    		while(rs.next()){
	    			
	    			GenericAction baseAction = null;
	    			
	    			// Pull the proper action type class
	    			ActionType actionType = ActionType.getByActionType(rs.getString("action_type"));
	    			
	    			plugin.debug("Found matching action type enum: " + actionType.getName());
    				
	    			if(actionType.isBlockAction()){
	    				BlockAction b = new BlockAction(null, null, null);
	    				baseAction = b;
	    			}
	    			else if( actionType.isEntityAction() ){
	    				EntityAction eka = new EntityAction(null, null, null);
	    				baseAction = eka;
	    			}
	    			else if( actionType.isItemStackAction() ){
	    				ItemStackAction isa = new ItemStackAction(null, null, null, null);
	    				baseAction = isa;
	    			}
	    			else if( actionType.isPlayerDeathAction() ){
	    				PlayerDeathAction pd = new PlayerDeathAction(null, null, null, null);
	    				baseAction = pd;
	    			}
	    			else if( actionType.isSignAction() ){
	    				SignAction sa = new SignAction(null, null, null, null);
	    				baseAction = sa;
	    			} else {
	    				plugin.log("Important: Action type '" + rs.getString("action_type") + "' has no official handling class, will be shown as generic." );
	    			}
	    			
	    			if(baseAction == null){
	    				baseAction = new GenericAction();
	    			}
	    				
    				// Set all shared values
    				baseAction.setType( actionType );
    				baseAction.setId( rs.getInt("id") );
    				baseAction.setAction_time( rs.getString("action_time") );
    				baseAction.setDisplay_date( rs.getString("display_date") );
    				baseAction.setDisplay_time( rs.getString("display_time") );
    				baseAction.setWorld_name( rs.getString("world") );
    				baseAction.setPlayer_name( rs.getString("player") );
    				baseAction.setX( rs.getInt("x") );
    				baseAction.setY( rs.getInt("y") );
    				baseAction.setZ( rs.getInt("z") );
    				baseAction.setData( rs.getString("data") );
    				
    				actions.add(baseAction);
	    			
	    		}
	    		
	    		rs.close();
	    		s.close();
	            plugin.conn.close();
	            
	        } catch (SQLException e) {
	            e.printStackTrace();
	        }
		}
		
		// Build result object
		QueryResult res = new QueryResult( actions, parameters );
		
		// Cache it if we're doing a lookup. Otherwise we don't
		// need a cache.
		if(parameters.getLookup_type().equals("lookup")){
			if(plugin.cachedQueries.containsKey(player.getName())){
				plugin.cachedQueries.remove(player.getName());
			}
			plugin.cachedQueries.put(player.getName(), res);
		}
		
		// Return it
		return res;
		
	}
	
	
	/**
	 * 
	 * @return
	 */
	public int delete( String beforeDateAlias ){
		int rows_affected = 0;
		String beforeDate = buildTimeCondition(beforeDateAlias,"<");
		if(!beforeDate.isEmpty()){
			try {
				String query = "DELETE FROM prism_actions WHERE 1=1" + beforeDate;
				plugin.debug("Deleting records prior to " + beforeDate + ": " + query);
				plugin.dbc();
				Statement s = plugin.conn.createStatement ();
				rows_affected = s.executeUpdate (query);
				s.close();
				plugin.conn.close();
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
		return rows_affected;
	}
	
	
	/**
	 * 
	 * @param args
	 */
	public String getArgumentConditions( QueryParameters parameters ){
		
		String query = "SELECT prism_actions.id, prism_actions.action_time, action_type, player, world, prism_actions.x, prism_actions.y, prism_actions.z, data, DATE_FORMAT(prism_actions.action_time, '%c/%e/%y') display_date, DATE_FORMAT(prism_actions.action_time, '%l:%i:%s %p') display_time FROM prism_actions";
		
		// If we're rolling back, we need to exclude records
		// at exact coords that have new entries there. So if
		// player A placed a block, then removes it, and player
		// B places a block at the same place, a rollback of
		// all player A's block-places won't damage what
		// player B added
		//
		// By default block-break rollbacks don't need this because
		// they won't restore when a new block is present.
//		String action_type = "";
//		if(parameters.getAction_type() != null && parameters.getAction_type().contains("block-break")){
//			action_type = "block-break";
//		}
//		if(parameters.getLookup_type().equals("rollback")){
//			query += " JOIN (" +
//						"SELECT x, y, z, max(action_time) as action_time" +
//						" FROM prism_actions" +
//						" GROUP BY x, y, z) latest" +
//						" ON prism_actions.action_time = latest.action_time" +
//						" AND prism_actions.x = latest.x" +
//						" AND prism_actions.y = latest.y" +
//						" AND prism_actions.z = latest.z";
//		}
		
		// World
		query += " WHERE 1=1";
		
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
			if(!parameters.getAllow_no_radius()){
				query += " AND world = '"+parameters.getWorld()+"'";
			}

			/**
			 * Actions
			 */
			String action_type = parameters.getAction_type();
			if(action_type != null){
				query += buildOrQuery("action_type", action_type.split(","));
			}
			
			/**
			 * Players
			 */
			String player = parameters.getPlayer();
			if(player != null){
				query += buildOrQuery("player", player.split(","));
			}
			
			/**
			 * Radius
			 */
			int radius = parameters.getRadius();
			if(radius > 0){
				query += buildRadiusCondition(radius, parameters.getPlayer_location());
			}
			
			/**
			 * Block
			 */
			String block = parameters.getBlock();
			if(block != null){
				query += buildOrQuery("data", block.split(","));
			}
			
			/**
			 * Entity
			 */
			String entity = parameters.getEntity();
			if(entity != null){
				query += buildOrQuery("data", entity.split(","));
			}
			
			/**
			 * Timeframe
			 */
			String time = parameters.getTime();
			if(time != null){
				query += buildTimeCondition(time,null);
			}
			
			/**
			 * Specific coords
			 */
			Location loc = parameters.getLoc();
			if(loc != null){
				query += " AND prism_actions.x = " +(int)loc.getBlockX()+ " AND prism_actions.y = " +(int)loc.getBlockY()+ " AND prism_actions.z = " +(int)loc.getBlockZ();
			}
			
			/**
			 * Order by
			 */
			String sort_dir = parameters.getSortDirection();
			query += " ORDER BY prism_actions.action_time "+sort_dir;
			
			/**
			 * LIMIT
			 */
			int limit = parameters.getLimit();
			if(limit > 0){
				query += " LIMIT 0,"+limit;
			}
		}

		plugin.debug("Query conditions: " + query);
		
		return query;
		
	}
	
	
	/**
	 * 
	 * @param fieldname
	 * @param arg_values
	 * @return
	 */
	protected String buildOrQuery( String fieldname, String[] arg_values ){
		String where = "";
		if(arg_values.length > 0){
			where += " AND (";
			int c = 1;
			for(String val : arg_values){
				if(c > 1 && c <= arg_values.length){
					where += " OR ";
				}
				where += fieldname + " = '"+val+"'";
				c++;
			}
			where += ")";
		}
		return where;
	}
	
	
	/**
	 * 
	 * @param arg_values
	 * @param player
	 * @return
	 */
	protected String buildRadiusCondition( int radius, Vector loc ){
		String where = "";
//		if(arg_values[0].equalsIgnoreCase("world")){
//			// @todo force bypassing max radius
//		} else {

			// @todo allow for worldedit selections

			//If the radius is set we need to format the min and max locations
			if (radius > 0) {

				//Check if location and world are supplied
				Vector minLoc = new Vector(loc.getX() - radius, loc.getY() - radius, loc.getZ() - radius);
				Vector maxLoc = new Vector(loc.getX() + radius, loc.getY() + radius, loc.getZ() + radius);
				
				where += " AND (prism_actions.x BETWEEN " + minLoc.getX() + " AND " + maxLoc.getX() + ")";
				where += " AND (prism_actions.y BETWEEN " + minLoc.getY() + " AND " + maxLoc.getY() + ")";
				where += " AND (prism_actions.z BETWEEN " + minLoc.getZ() + " AND " + maxLoc.getZ() + ")";

			}
//		}
		return where;
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String buildTimeCondition( String arg_value, String equation ){
		
		String where = "";

		int type = 2;
		for (int j = 0; j < arg_value.length(); j++) {
			String c = arg_value.substring(j, j+1);
			if (!TypeUtils.isNumeric(c)) {
				if (c.equals("m") || c .equals("s") || c.equals("h") || c.equals("d") || c.equals("w"))
					type = 0;
				if (c.equals("-") || c.equals(":"))
					type = 1;
			}
		}
		
		String dateFrom = null;

		//If the time is in the format '0w0d0h0m0s'
		if (type == 0) {

			int weeks = 0;
			int days = 0;
			int hours = 0;
			int mins = 0;
			int secs = 0;

			String nums = "";
			for (int j = 0; j < arg_value.length(); j++) {
				String c = arg_value.substring(j, j+1);
				if (TypeUtils.isNumeric(c)){
					nums += c;
					continue;
				}
				int num = Integer.parseInt(nums);
				if (c.equals("w")) weeks = num;
				else if (c.equals("d")) days = num;
				else if (c.equals("h")) hours = num;
				else if (c.equals("m")) mins = num;
				else if (c.equals("s")) secs = num;
				else throw new IllegalArgumentException("Invalid time measurement: " + c);
				nums = "";
			}

			Calendar cal = Calendar.getInstance();
			cal.add(Calendar.WEEK_OF_YEAR, -1 * weeks);
			cal.add(Calendar.DAY_OF_MONTH, -1 * days);
			cal.add(Calendar.HOUR, -1 * hours);
			cal.add(Calendar.MINUTE, -1 * mins);
			cal.add(Calendar.SECOND, -1 * secs);
			
			SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			dateFrom = form.format(cal.getTime());

		}
		//Invalid time format
		else if (type == 2)
			throw new IllegalArgumentException("Invalid time format!");
		
		
		if(dateFrom != null){
			if(equation == null){
				where += " AND action_time >= '" + dateFrom + "'";
			} else {
				where += " AND action_time "+equation+" '" + dateFrom + "'";
			}
		}
		
		return where;
		
	}
}