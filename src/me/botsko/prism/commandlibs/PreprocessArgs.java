package me.botsko.prism.commandlibs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map.Entry;

import me.botsko.prism.MaterialAliases;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.bridge.WorldEditBridge;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.LevenshteinDistance;
import me.botsko.prism.utils.TypeUtils;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class PreprocessArgs {
	
	
	/**
	 * 
	 * @param sender
	 * @param msg
	 */
	protected static void respond( CommandSender sender, String msg ){
		if(sender != null){
			sender.sendMessage(msg);
		}
	}
	
	
	/**
	 * 
	 * @param args
	 */
	public static QueryParameters process( Prism plugin, CommandSender sender, String[] args, PrismProcessType processType, int starting ){
		
		Player player = null;
		if(sender != null && sender instanceof Player){
			player = (Player) sender;
		}
		
		QueryParameters parameters = new QueryParameters();
		HashMap<String,String> foundArgs = new HashMap<String,String>();
		
		parameters.setLookup_type(processType);
		
		if(args != null){
		
			// Iterate over arguments
			for (int i = starting; i < args.length; i++) {
				
				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				// Verify we have an arg we can match
				String[] possibleArgs = {"a","r","t","p","w","b","e","-"};
				if(!Arrays.asList(possibleArgs).contains(arg.substring(0,1))){
					respond( sender, plugin.playerError("Unrecognized parameter '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				
				// Verify they're formatting like a:[val] or like -arg
				if(!(arg.contains(":") || arg.contains("-"))){
					respond( sender, plugin.playerError("Missing or invalid parameter value for '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				if (!(arg.substring(1,2).equals(":") || arg.substring(0,1).equals("-"))){
					respond( sender, plugin.playerError("Misplaced colon for '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				
				// Split parameter and values
				String arg_type = arg.toLowerCase().substring(0,1);
				String val = arg.substring(1,2).equals(":") ? arg.toLowerCase().substring(2) : arg.toLowerCase().substring(1);
				
				if(val.isEmpty()){
					respond( sender, plugin.playerError("Can't use empty values for '"+arg+"'. Use /prism ? for help.") );
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
							ArrayList<ActionType> actionTypes = ActionType.getByActionsType( action.replace("!", "") );
							if(!actionTypes.isEmpty()){
								for(ActionType actionType : actionTypes){
									MatchRule match = MatchRule.INCLUDE;
									if(action.startsWith("!")){
										match = MatchRule.EXCLUDE;
									}
									parameters.addActionType( actionType, match );
								}
							} else {
								
								// Remove the arg. If only one action provided, this will essentially
								// ensure a validation error prevents a query.
								for (Entry<String, String> entry : foundArgs.entrySet()){
								    if( entry.getKey().equals(arg_type) && entry.getValue().equals( val ) ){
								    	foundArgs.remove(entry.getKey());
								    }
								}
								respond( sender, plugin.playerError("Ignoring action '"+action.replace("!", "")+"' because it's unrecognized. Did you mean '" + LevenshteinDistance.getClosestAction(action) +"'? Type '/prism params' for help.") );
								if(actions.length == 1){
									return null;
								}
							}
						}
					}
				}
				
				// Player
				if(arg_type.equals("p")){
					String[] playerNames = val.split(",");
					if(playerNames.length > 0){
						for(String playerName : playerNames){
							MatchRule match = MatchRule.INCLUDE;
							if(playerName.startsWith("!")){
								match = MatchRule.EXCLUDE;
							}
							parameters.addPlayerName( playerName.replace("!", ""), match );
						}
					}
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
							respond( sender, plugin.playerError("Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help.") );
							return null;
						}
						if(radius > plugin.getConfig().getInt("prism.max-radius-unless-overridden")){
							radius = plugin.getConfig().getInt("prism.max-radius-unless-overridden");
							respond( sender, plugin.playerError("Forcing radius to " + radius + " as allowed by config.") );
						}
						if(radius > 0){
							parameters.setRadius( radius );
						}
					} else {
						
						// User wants an area inside of a worldedit selection
						if(val.equals("we")){
							
							if (plugin.plugin_worldEdit == null) {
								respond( sender, plugin.playerError("This feature is disabled because Prism couldn't find WorldEdit.") );
								return null;
							} else {
							
								// Load a selection from world edit as our area.
								if(player != null){
									parameters = WorldEditBridge.getSelectedArea(plugin, player, parameters);
								}
							}
						}
						
						// User has asked for a global radius
						else if(val.equals("global")){
							if(plugin.getConfig().getBoolean("prism.limit-global-radius-override-to-lookups")){
								if( parameters.getLookup_type().equals(PrismProcessType.LOOKUP)){
									parameters.setAllow_no_radius(true);
								} else {
									respond( sender, plugin.playerError("Current configuration limits global radius to lookups.") );
									return null;
								}
							} else {
								// Allow no matter what
								parameters.setAllow_no_radius(true);
							}
						} else {
							respond( sender, plugin.playerError("Radius must be a number, 'global', or 'we'. Use /prism ? for a assitance.") );
							return null;
						}
					}
				}
				
				// Entity
				if(arg_type.equals("e")){
					String[] entityNames = val.split(",");
					if(entityNames.length > 0){
						for(String entityName : entityNames){
							MatchRule match = MatchRule.INCLUDE;
							if(entityName.startsWith("!")){
								match = MatchRule.EXCLUDE;
							}
							parameters.addEntity( entityName.replace("!", ""), match );
						}
					}
				}
				
				// Block
				if(arg_type.equals("b")){
					
					String[] blocks = val.split(",");
					
					if(blocks.length > 0){
						
						String block_match = "block_id\":%s,";
						String block_subid_match = "\"block_subid\":%s";
						
						for(String b : blocks){
					
							// if user provided id:subid
							if(b.contains(":") && b.length() >= 3){
								String[] ids = b.split(":");
								if(ids.length == 2 && TypeUtils.isNumeric(ids[0]) && TypeUtils.isNumeric(ids[1])){
									parameters.addBlockFilter( String.format(block_match+block_subid_match, ids[0], ids[1]) );
								} else {
									respond( sender, plugin.playerError("Invalid block filter '"+val+"'. Use /prism ? [command] for help.") );
									return null;
								}
							} else {
								
								// It's id without a subid
								if(TypeUtils.isNumeric(b)){
									parameters.addBlockFilter( String.format(block_match, b, "0") );
								} else {
									
									// Lookup the item name, get the ids
									MaterialAliases items = plugin.getItems();
									ArrayList<int[]> itemIds = items.getItemIdsByAlias( b );
									if(itemIds.size() > 0){
										for(int[] ids : itemIds){
											if(ids.length == 2){
												// If we really care about the sub id because it's a whole different item
												if(BlockUtils.hasSubitems(ids[0])){
													parameters.addBlockFilter( String.format(block_match+block_subid_match, ids[0], ids[1]) );
												} else {
													parameters.addBlockFilter( String.format(block_match, ids[0]) );
												}
											}
										}
									}
								}
							}
						}
					}
				}
				
				// Time
				if(arg_type.equals("t")){
					String date = translateTimeStringToDate(plugin,sender,val);
					if(date != null){
						parameters.setTime( date );
					} else {
						return null;
					}
				}
				
				// Special Flags
				if(arg_type.equals("-")){
					try {
						String[] flagComponents = val.split("=");
						Flag flag = Flag.valueOf( flagComponents[0].replace("-", "_").toUpperCase() );
						if(!(parameters.hasFlag(flag))){
							if( plugin.getConfig().getString("prism.database.mode").equalsIgnoreCase("sqlite") && flag.equals(Flag.NO_OVERWRITE)){
								respond( sender, plugin.playerError("-no-overwrite is not currently supported on sqlite databases.") );
								return null;
							} else {
								
								parameters.addFlag(flag);
								
								// Flag has a value
								if( flagComponents.length > 1 ){
									if(flag.equals(Flag.PER_PAGE)){
										if(TypeUtils.isNumeric(flagComponents[1])){
											parameters.setPerPage( Integer.parseInt(flagComponents[1]) );
										} else {
											respond( sender, plugin.playerError("Per-page flag value must be a number. Use /prism ? [command] for help.") );
											return null;
										}
									}
								} 
							}
						}
					} catch(IllegalArgumentException ex){
						respond( sender, plugin.playerError("Unrecognized flag '"+val+"'. Use /prism ? [command] for help.") );
						return null;
					}
				}
			}
			
			// Validate any required args are set
			if(foundArgs.isEmpty()){
				respond( sender, plugin.playerError("You're missing parameters. Use /prism ? for a assitance.") );
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
					parameters.addDefaultUsed( "r:" + parameters.getRadius() );
				}
			}
			// World default
			if(!foundArgs.containsKey("w")){
				if(player != null){
					parameters.setWorld( player.getWorld().getName() );
				}
			}
			// Time default
			if(!foundArgs.containsKey("t")){
				String date = translateTimeStringToDate(plugin,sender,plugin.getConfig().getString("prism.default-time-since"));
				if(date == null){
					plugin.log("Error - date range configuration for prism.time-since is not valid");
					date = translateTimeStringToDate(plugin,sender,"3d");
				}
				parameters.setTime(date);
				parameters.addDefaultUsed( "t:" + plugin.getConfig().getString("prism.default-time-since") );
			}
			
			// Player location
			if(player != null){
				parameters.setMinMaxVectorsFromPlayerLocation( player.getLocation() );
			}
		}
		return parameters;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public static String translateTimeStringToDate( Prism plugin, CommandSender sender, String arg_value ){
		
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
					respond( sender, plugin.playerError("Invalid time value '"+c+"'. Use /prism ? for a help.") );
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
			respond( sender, plugin.playerError("Invalid timeframe values. Use /prism ? for a help.") );
			return null;
		}
		
		return dateFrom;
		
	}

}
