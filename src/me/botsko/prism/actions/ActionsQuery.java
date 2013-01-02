package me.botsko.prism.actions;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;

import org.bukkit.Location;
import org.bukkit.entity.Player;
import org.bukkit.util.Vector;

import me.botsko.prism.Prism;
import me.botsko.prism.actiontypes.ActionType;
import me.botsko.prism.utils.TypeUtils;

public class ActionsQuery {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private HashMap<String,String> foundArgs = new HashMap<String,String>();
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public ActionsQuery(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * If we receive a player and arguments, it's being run from the
	 * command line. We must convert these into a QueryParameter class
	 * so we can pass that object in.
	 * @return
	 */
	public List<Action> rollback( Player player, String[] args ){
		
		// Pull results
		List<Action> actions = new ArrayList<Action>();
				
		QueryParameters parameters = preprocessArguments( player, args );
		parameters.setLookup_type("rollback");
		
		// If no args, return
		if(foundArgs.isEmpty()){
			return actions;
		}
		return lookup(parameters);
	}
	
	
	/**
	 * If we receive a player and arguments, it's being run from the
	 * command line. We must convert these into a QueryParameter class
	 * so we can pass that object in.
	 * @return
	 */
	public List<Action> lookup( Player player, String[] args ){
		
		// Pull results
		List<Action> actions = new ArrayList<Action>();
				
		QueryParameters parameters = preprocessArguments( player, args );
		
		// If no args, return
		if(foundArgs.isEmpty()){
			return actions;
		}
		return lookup(parameters);
	}
	
	
	/**
	 * 
	 * @return
	 */
	public List<Action> lookup( QueryParameters parameters ){
		
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
	    			ActionType actionType = plugin.getActionType(rs.getString("action_type"));
    				

	    			
	    			// @todo this needs more cleanup
	    			String[] possibleArgs = {"block-break","block-place","block-burn","block-fade","block-ignite","flint-steel","tree-grow","mushroom-grow","leaf-decay","entity-explode"};
	    			if(Arrays.asList(possibleArgs).contains(rs.getString("action_type"))){
	    				BlockAction b = new BlockAction(null, null, null);
	    				baseAction = b;
	    			}
	    			else if( rs.getString("action_type").equals("entity-kill") ){
	    				EntityAction eka = new EntityAction(null, null, null);
	    				baseAction = eka;
	    			} else {
	    				plugin.log("Error: Unhandled action type: " + rs.getString("action_type") );
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
		return actions;
	}
	
	
	/**
	 * 
	 * @param args
	 */
	protected QueryParameters preprocessArguments( Player player, String[] args ){
		
		QueryParameters parameters = new QueryParameters();
		
		if(args != null){
		
			// Iterate over arguments
			for (int i = 1; i < args.length; i++) {
				
				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				// Verify they're formatting like a:[val]
				if(!arg.contains(":")){
					throw new IllegalArgumentException("Invalid argument format: " + arg);
				}
				if (!arg.substring(1,2).equals(":")) {
					throw new IllegalArgumentException("Invalid argument format: " + arg);
				}
				
				// Split parameter and values
				String arg_type = arg.substring(0,1).toLowerCase();
				String val = arg.substring(2);
				String[] possibleArgs = {"a","r","t","p","w","b","e"};
				if(Arrays.asList(possibleArgs).contains(arg_type)){
					if(!val.isEmpty()){
						plugin.debug("Found arg type " + arg_type + " with value: " + val);
						foundArgs.put(arg_type, val);
					} else {
						throw new IllegalArgumentException("You must supply at least one argument.");
					}
				}
				
				// Action
				if(arg_type.equals("a")){
					parameters.setAction_type( val );
				}
				
				// Player
				if(arg_type.equals("p")){
					parameters.setPlayer( val );
				}
				
				// World
				if(arg_type.equals("w")){
					parameters.setWorld( val );
				}
				
				// Radius
				if(arg_type.equals("r")){
					if(TypeUtils.isNumeric(val)){
						parameters.setRadius( Integer.parseInt(val) );
					} else {
						throw new IllegalArgumentException("Invalid argument format: " + arg);
					}
				}
				
				// Entity
				if(arg_type.equals("e")){
					parameters.setEntity( val );
				}
				
				// Block
				if(arg_type.equals("b")){
					parameters.setBlock( val );
				}
				
				// Time
				if(arg_type.equals("t")){
					parameters.setTime( val );
				}
			}
			
			// Validate any required args are set
			if(foundArgs.isEmpty()){
				throw new IllegalArgumentException("You must supply at least one argument.");
			}
			
			/**
			 * Set defaults
			 */
			// Radius default
			if(!foundArgs.containsKey("r")){
				plugin.debug("Setting default radius to " + plugin.getConfig().getString("default-radius"));
				parameters.setRadius( Integer.parseInt( plugin.getConfig().getString("prism.default-radius") ) );
				
				// Add the default radius to the foundArgs so even a parameter-less query will
				// return something.
				// @todo this should only happen on lookup
//				foundArgs.put("r", plugin.getConfig().getString("prism.default-radius"));
				
			}
			// World default
			if(!foundArgs.containsKey("w")){
				parameters.setWorld( player.getWorld().getName() );
			}
			// Player location
			parameters.setPlayer_location( player.getLocation().toVector() );
		}
		return parameters;
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
		if(parameters.getLookup_type().equals("rollback") && parameters.getAction_type().contains("block-place")){
			query += " JOIN (" +
						"SELECT x, y, z, max(action_time) as action_time" +
						" FROM prism_actions" +
						" GROUP BY x, y, z) latest" +
						" ON prism_actions.action_time = latest.action_time" +
						" AND prism_actions.x = latest.x" +
						" AND prism_actions.y = latest.y" +
						" AND prism_actions.z = latest.z";
		}
		
		// World
		query += " WHERE world = '"+parameters.getWorld()+"'";

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
		 * Timeframe
		 */
		String time = parameters.getTime();
		if(time != null){
			query += buildTimeCondition(time);
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
		String where = " AND (";
		int c = 1;
		for(String val : arg_values){
			if(c > 1 && c <= arg_values.length){
				where += " OR ";
			}
			where += fieldname + " = '"+val+"'";
			c++;
		}
		where += ")";
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
	protected String buildTimeCondition( String arg_value ){
		
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
			where += " AND action_date >= " + dateFrom;
		}
		
		return where;
		
	}
}