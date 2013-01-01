package me.botsko.prism.actions;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

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
	public List<Action> lookup( Player player, String[] args ){
		
		String where = getArgumentConditions(player,args);
		
		List<Action> actions = new ArrayList<Action>();
		try {
            
			plugin.dbc();
			
            PreparedStatement s;
    		s = plugin.conn.prepareStatement ("SELECT * FROM prism_actions" + where + " ORDER BY action_time DESC");
    		s.executeQuery();
    		ResultSet rs = s.getResultSet();
    		
    		while(rs.next()){
    			
    			// @todo this needs major cleanup
    			if( rs.getString("action_type").equals("block-break") 
    					|| rs.getString("action_type").equals("block-place")
    					|| rs.getString("action_type").equals("block-burn")
    					|| rs.getString("action_type").equals("block-fade")
    					|| rs.getString("action_type").equals("block-ignite")
    					|| rs.getString("action_type").equals("flint-steel")
    					|| rs.getString("action_type").equals("tree-grow")
    					|| rs.getString("action_type").equals("mushroom-grow")){
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
		return actions;
	}
	
	
	/**
	 * 
	 * @param args
	 */
	public String getArgumentConditions( Player player, String[] args ){
		
		String where = " WHERE 1=1";
		
		
		// @todo probs: need to validate certain args are present, add defaults if not

		
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
			String[] arg_values = arg.substring(2).split(",");
			
			
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
				
				if(arg_values[0].equalsIgnoreCase("world")){
					// @todo force bypassing max radius
				} else {
				
					if(!TypeUtils.isNumeric(arg_values[0])){
						throw new IllegalArgumentException("Not an integer " + arg_values[0]);
					}
					
					int radius = Integer.parseInt(arg_values[0]);
			
					// @todo allow for worldedit selections
	
					// Check if there is a max radius
	//				if (radius == null && Config.MaxRadius != 0) radius = Config.MaxRadius;
					
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
			}
			
			
			/**
			 * Time frame
			 */
			else if(arg_type.equalsIgnoreCase("t")){
				
				int type = 2;
				for (int j = 0; j < arg.length(); j++) {
					String c = arg.substring(j, j+1);
					if (!TypeUtils.isNumeric(c)) {
						if (c.equals("m") || c .equals("s") || c.equals("h") || c.equals("d") || c.equals("w"))
							type = 0;
						if (c.equals("-") || c.equals(":"))
							type = 1;
					}
				}
				
				String dateFrom = null;
				String dateTo = null;

				//If the time is in the format '0w0d0h0m0s'
				if (type == 0) {

					int weeks = 0;
					int days = 0;
					int hours = 0;
					int mins = 0;
					int secs = 0;

					String nums = "";
					for (int j = 0; j < arg.substring(2).length(); j++) {
						String c = arg.substring(2).substring(j, j+1);
						if (TypeUtils.isNumeric(c)) {
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
				//If the time is in the format 'yyyy-MM-dd HH:mm:ss'
				else if (type == 1) {
					if (arg_values.length == 1) {
						SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd");
						dateFrom = form.format(Calendar.getInstance().getTime()) + " " + arg_values[0];
					}
					if (arg_values.length >= 2)
						dateFrom = arg_values[0] + " " + arg_values[1];
					if (arg_values.length == 4)
						dateTo = arg_values[2] + " " + arg_values[3];
				}
				//Invalid time format
				else if (type == 2)
					throw new IllegalArgumentException("Invalid time format!");
				
				
				if(dateFrom != null){
					where += " AND action_date >= " + dateFrom;
				}
				if(dateTo != null){
					where += " AND action_date <= " + dateTo;
				}
			}
			
			plugin.debug("Found arg type " + arg_type + " with value: " + arg.substring(2));
			plugin.debug("Query conditions: " + where);
			
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
}