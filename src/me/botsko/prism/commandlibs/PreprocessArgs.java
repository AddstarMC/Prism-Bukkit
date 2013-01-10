package me.botsko.prism.commandlibs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map.Entry;

import me.botsko.prism.MaterialAliases;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.utils.LevenshteinDistance;
import me.botsko.prism.utils.TypeUtils;

import org.bukkit.entity.Player;

public class PreprocessArgs {
	
	
	/**
	 * 
	 * @param args
	 */
	public static QueryParameters process( Prism plugin, Player player, String[] args, PrismProcessType processType, int starting ){
		
		QueryParameters parameters = new QueryParameters();
		HashMap<String,String> foundArgs = new HashMap<String,String>();
		
		parameters.setLookup_type(processType);
		
		if(args != null){
		
			// Iterate over arguments
			for (int i = starting; i < args.length; i++) {
				
				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				// Verify we have an arg we can match
				String[] possibleArgs = {"a","r","t","p","w","b","e"};
				if(!Arrays.asList(possibleArgs).contains(arg.substring(0,1))){
					player.sendMessage( plugin.playerError("Unrecognized parameter '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				
				// Verify they're formatting like a:[val]
				if(!arg.contains(":")){
					player.sendMessage( plugin.playerError("Missing parameter value for '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				if (!arg.substring(1,2).equals(":")) {
					player.sendMessage( plugin.playerError("Misplaced colon for '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				
				// Split parameter and values
				String arg_type = arg.toLowerCase().substring(0,1);
				String val = arg.toLowerCase().substring(2);
				
				if(val.isEmpty()){
					player.sendMessage( plugin.playerError("Can't use empty values for '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				
				// Officially certify we found a valid argument and value!
				if(Arrays.asList(possibleArgs).contains(arg_type)){
					foundArgs.put(arg_type, val);
					parameters.setFoundArgs(foundArgs);
				}
				
				// Action
				if(arg_type.equals("a")){
					String[] actions = val.split(",");
					if(actions.length > 0){
						for(String action : actions){
							// Find all actions that match the action provided - whether the full name or
							// short name.
							ArrayList<ActionType> actionTypes = ActionType.getByActionsType( action );
							if(!actionTypes.isEmpty()){
								for(ActionType actionType : actionTypes){
									parameters.addActionType( actionType );
								}
							} else {
								
								// Remove the arg. If only one action provided, this will essentially
								// ensure a validation error prevents a query.
								for (Entry<String, String> entry : foundArgs.entrySet()){
								    if( entry.getKey().equals(arg_type) && entry.getValue().equals( val ) ){
								    	foundArgs.remove(entry.getKey());
								    }
								}
								player.sendMessage( plugin.playerError("Ignoring action '"+action+"' because it's unrecognized. Did you mean '" + LevenshteinDistance.getClosestAction(action) +"'? Type '/prism params' for help.") );
							}
						}
					}
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
						int radius = Integer.parseInt(val);
						if(radius <= 0){
							player.sendMessage( plugin.playerError("Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help.") );
						}
						if(radius > plugin.getConfig().getInt("prism.max-radius-unless-overridden")){
							radius = plugin.getConfig().getInt("prism.max-radius-unless-overridden");
							player.sendMessage( plugin.playerError("Forcing radius to " + radius + " as allowed by config.") );
						}
						if(radius > 0){
							parameters.setRadius( radius );
						}
					} else {
						// User has asked for a global radius
						if(val.equals("global")){
							if( parameters.getLookup_type().equals("lookup")){
								parameters.setAllow_no_radius(true);
							} else {
								player.sendMessage( plugin.playerError("A global radius may only be used on lookup.") );
								return null;
							}
						} else {
							player.sendMessage( plugin.playerError("Radius must be a number or 'global'. Use /prism ? for a assitance.") );
							return null;
						}
					}
				}
				
				// Entity
				if(arg_type.equals("e")){
					parameters.setEntity( val );
				}
				
				// Block
				if(arg_type.equals("b")){
					
					String[] blocks = val.split(",");
					
					if(blocks.length > 0){
						
						String block_match = "{\"block_id\":%d,\"block_subid\":%d}";
						
						for(String b : blocks){
					
							// if user provided id:subid
							if(b.contains(":") && b.length() >= 3){
								String _tmp_id = b.substring(0,1);
								String _tmp_subid = b.substring(2);
								if(!TypeUtils.isNumeric(_tmp_id) || !TypeUtils.isNumeric(_tmp_subid)){
									parameters.addBlockFilter( String.format(block_match, _tmp_id, _tmp_subid) );
								}
							} else {
								
								// It's id without a subid
								if(TypeUtils.isNumeric(b)){
									parameters.addBlockFilter( String.format(block_match, b, 0) );
								} else {
									
									// Lookup the item name, get the ids
									MaterialAliases items = plugin.getItems();
									int[] ids = items.getItemIdsByAlias( b );
									if(ids.length == 2){
										parameters.addBlockFilter( String.format(block_match, ids[0], ids[1]) );
									}
								}
							}
						}
					}
				}
				
				// Time
				if(arg_type.equals("t")){
					String date = translateTimeStringToDate(plugin,player,val);
					if(date != null){
						parameters.setTime( date );
					} else {
						return null;
					}
				}
			}
			
			// Validate any required args are set
			if(foundArgs.isEmpty()){
				player.sendMessage( plugin.playerError("You're missing parameters. Use /prism ? for a assitance.") );
				return null;
			}
			
			/**
			 * Set defaults
			 */
			// Radius default
			if(!foundArgs.containsKey("r")){
				if(parameters.getAllow_no_radius()){
					// We'll allow no radius.
				} else {
					parameters.setRadius( plugin.getConfig().getInt("prism.default-radius") );
				}
			}
			// World default
			if(!foundArgs.containsKey("w")){
				parameters.setWorld( player.getWorld().getName() );
			}
			// Player location
			parameters.setPlayerLocation( player.getLocation() );
		}
		return parameters;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public static String translateTimeStringToDate( Prism plugin, Player player, String arg_value ){
		
		String dateFrom = null;

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
				else {
					if(player != null){
						player.sendMessage( plugin.playerError("Invalid time value '"+c+"'. Use /prism ? for a help.") );
					}
					return null;
				}
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
		else if (type == 2){
			if(player != null){
				player.sendMessage( plugin.playerError("Invalid timeframe values. Use /prism ? for a help.") );
			}
			return null;
		}
		
		return dateFrom;
		
	}

}
