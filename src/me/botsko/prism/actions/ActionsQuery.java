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
import java.util.Map.Entry;

import org.bukkit.entity.Player;
import org.bukkit.util.Vector;

import me.botsko.prism.Prism;
import me.botsko.prism.utils.TypeUtils;

public class ActionsQuery {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private HashMap<String,String> foundArgs;
	
	
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
	public List<Action> lookup( Player player, String where ){
		return lookup(player, null, where);
	}
	
	
	/**
	 * 
	 * @return
	 */
	public List<Action> lookup( Player player, String[] args, String where ){
		
		// Pull results
		List<Action> actions = new ArrayList<Action>();
		
		// Preprocess all args and validate them
		preprocessArguments(args);
		
		// Build conditions based off final args
		if(where == null) where = getArgumentConditions(player,args);
		
		if(where!= null){
			try {
	            
				plugin.dbc();
				
	            PreparedStatement s;
	    		s = plugin.conn.prepareStatement ("SELECT * FROM prism_actions" + where + " ORDER BY action_time DESC");
	    		s.executeQuery();
	    		ResultSet rs = s.getResultSet();
	    		
	    		while(rs.next()){
	    			
	    			// @todo this needs more cleanup
	    			String[] possibleArgs = {"block-break","block-place","block-burn","block-fade","block-ignite","flint-steel","tree-grow","mushroom-grow"};
	    			if(Arrays.asList(possibleArgs).contains(rs.getString("action_type"))){
		    			actions.add( new BlockAction(
		    					rs.getString("action_time"),
		    					rs.getString("action_type"),
		    					rs.getString("world"),
		    					rs.getString("player"),
		    					rs.getInt("x"),
		    					rs.getInt("y"),
		    					rs.getInt("z"),
		    					rs.getString("data")
		    				) );
	    			}
	    			
	    			else if( rs.getString("action_type").equals("entity-kill") ){
		    			actions.add( new EntityKillAction(
		    					rs.getString("action_time"),
		    					rs.getString("action_type"),
		    					rs.getString("world"),
		    					rs.getString("player"),
		    					rs.getInt("x"),
		    					rs.getInt("y"),
		    					rs.getInt("z"),
		    					rs.getString("data")
		    				) );
	    			} else {
	    				plugin.log("Error: Unhandled action type: " + rs.getString("action_type") );
	    			}
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
	protected void preprocessArguments( String[] args ){
		
		foundArgs = new HashMap<String,String>();
		
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
				
				// Split parameter and value, split values by commas
				String arg_type = arg.substring(0,1).toLowerCase();
				String[] possibleArgs = {"a","r","t","p","w","b","e"};
				if(Arrays.asList(possibleArgs).contains(arg_type)){
					String val = arg.substring(2);
					if(!val.isEmpty()){
						foundArgs.put(arg_type, val);
					} else {
						throw new IllegalArgumentException("You must supply at least one argument.");
					}
				}
			}
			
			// Validate any required args are set
			if(foundArgs.isEmpty()){
				throw new IllegalArgumentException("You must supply at least one argument.");
			}
			
			// Set defaults
			if(!foundArgs.containsKey("r")){
				plugin.debug("Setting default radius to " + plugin.getConfig().getString("default-radius"));
				foundArgs.put("r", plugin.getConfig().getString("prism.default-radius"));
			}
		}
	}
	
	
	/**
	 * 
	 * @param args
	 */
	public String getArgumentConditions( Player player, String[] args ){
		
		String where = null;
		
		if(!foundArgs.isEmpty()){
			
			// Begin base condition
			where = " WHERE 1=1";
			
			// Iterate final arguments
			for (Entry<String, String> entry : foundArgs.entrySet()){
				// entry.getKey() entry.getValue()
				
				String arg_type = entry.getKey();
				String[] arg_values = entry.getValue().split(",");
			   

				/**
				 * Actions
				 */
				if(arg_type.equalsIgnoreCase("a")){
					where += buildOrQuery("action_type", arg_values);
				}
				
				
				/**
				 * Players
				 */
				else if(arg_type.equalsIgnoreCase("p")){
					where += buildOrQuery("player", arg_values);
				}
				
				
				/**
				 * World
				 */
				else if(arg_type.equalsIgnoreCase("w")){
					where += buildOrQuery("world", arg_values);
				}
			
			
				/**
				 * Radius
				 */
				else if(arg_type.equalsIgnoreCase("r")){
					where += buildRadiusCondition(arg_values, player);
				}
				
				
				/**
				 * Timeframe
				 */
				else if(arg_type.equalsIgnoreCase("t")){
					where += buildTimeCondition(arg_values[0]);
				}
			
				plugin.debug("Found arg type " + arg_type + " with value: " + entry.getValue());
				plugin.debug("Query conditions: " + where);

			}
		}
		
		return where;
		
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
	protected String buildRadiusCondition( String[] arg_values, Player player ){
		String where = "";
		if(arg_values[0].equalsIgnoreCase("world")){
			// @todo force bypassing max radius
		} else {
		
			if(!TypeUtils.isNumeric(arg_values[0])){
				throw new IllegalArgumentException("Not an integer " + arg_values[0]);
			}
			
			int radius = Integer.parseInt(arg_values[0]);
	
			// @todo allow for worldedit selections

			//If the radius is set we need to format the min and max locations
			if (radius > 0) {

				//Check if location and world are supplied
				Vector loc = player.getLocation().toVector();
				Vector minLoc = new Vector(loc.getX() - radius, loc.getY() - radius, loc.getZ() - radius);
				Vector maxLoc = new Vector(loc.getX() + radius, loc.getY() + radius, loc.getZ() + radius);
				
				where += " AND (x BETWEEN " + minLoc.getX() + " AND " + maxLoc.getX() + ")";
				where += " AND (y BETWEEN " + minLoc.getY() + " AND " + maxLoc.getY() + ")";
				where += " AND (z BETWEEN " + minLoc.getZ() + " AND " + maxLoc.getZ() + ")";

			}
		}
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