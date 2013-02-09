package me.botsko.prism.commandlibs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
		} else {
//			System.out.print(msg); // @todo let this output to console, is useful for db purge debugging
		}
	}
	
	
	/**
	 * 
	 * @param args
	 */
	public static QueryParameters process( Prism plugin, CommandSender sender, String[] args, PrismProcessType processType, int startAt ){
		
		Player player = null;
		if(sender != null && sender instanceof Player){
			player = (Player) sender;
		}
		
		QueryParameters parameters = new QueryParameters();
		ConcurrentHashMap<String,String> foundArgs = new ConcurrentHashMap<String,String>();
		
		parameters.setProcessType(processType);
		
		if(args != null){

			// Iterate over arguments
			for (int i = startAt; i < args.length; i++) {
				
				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				// Verify they're formatting like a:[val] or like -arg
				if(!(arg.contains(":") || arg.contains("-"))){
					respond( sender, plugin.playerError("Missing or invalid parameter value for '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				if (!(arg.contains(":") || arg.substring(0,1).equals("-"))){
					respond( sender, plugin.playerError("Misplaced colon for '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				
				// Split parameter and values
				String[] argEntry = arg.toLowerCase().split(":");
				String arg_type = argEntry[0];
				String val = arg.contains(":") ? arg.replace(argEntry[0] + ":", "") : argEntry[0];
				
				// Verify we have an arg we can match
				String[] possibleArgs = {"a","r","t","p","w","b","e","k","before","since","-"};
				if(!Arrays.asList(possibleArgs).contains(arg_type) && !arg_type.startsWith("-")){
					respond( sender, plugin.playerError("Unrecognized parameter '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				
				// Verify no empty val
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
									
									// Ensure the action allows this process type
									if( (processType.equals(PrismProcessType.ROLLBACK) && !actionType.canRollback()) || (processType.equals(PrismProcessType.RESTORE) && !actionType.canRestore()) ){
										respond( sender, plugin.playerError("Ingoring action '"+action.replace("!", "")+"' because it doesn't support rollbacks.") );
										continue;
									}
									
									// Check match type
									MatchRule match = MatchRule.INCLUDE;
									if(action.startsWith("!")){
										match = MatchRule.EXCLUDE;
									}
									parameters.addActionType( actionType, match );
								}
							} else {
								respond( sender, plugin.playerError("Ignoring action '"+action.replace("!", "")+"' because it's unrecognized. Did you mean '" + LevenshteinDistance.getClosestAction(action) +"'? Type '/prism params' for help.") );
							}
						}
						// If none were valid, we end here.
						if(parameters.getActionTypes().size() == 0){
							return null;
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
					if(TypeUtils.isNumeric(val) || (val.contains(":") && val.split(":").length >= 1 && TypeUtils.isNumeric(val.split(":")[1]))){
						int radius = 0;
						if(val.contains(":")){
							radius = Integer.parseInt(val.split(":")[1]);
							String radiusPlayer = val.split(":")[0];
							if(plugin.getServer().getPlayer(radiusPlayer) != null){
								player = plugin.getServer().getPlayer(radiusPlayer);
							} else {
								respond( sender, plugin.playerError("Couldn't find the player named '" + radiusPlayer + "'. Perhaps they are not online or you misspelled their name?") );
								return null;
							}
						} else {
							radius = Integer.parseInt(val);
						}
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
								if( parameters.getProcessType().equals(PrismProcessType.LOOKUP)){
									parameters.setAllowNoRadius(true);
								} else {
									respond( sender, plugin.playerError("Current configuration limits global radius to lookups.") );
									return null;
								}
							} else {
								// Allow no matter what
								parameters.setAllowNoRadius(true);
							}
						} else {
							respond( sender, plugin.playerError("Radius must be a number, 'global', 'player:number', or 'we'. Use /prism ? for a assitance.") );
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
						for(String b : blocks){
					
							// if user provided id:subid
							if(b.contains(":") && b.length() >= 3){
								String[] ids = b.split(":");
								if(ids.length == 2 && TypeUtils.isNumeric(ids[0]) && TypeUtils.isNumeric(ids[1])){
									parameters.addBlockFilter( Integer.parseInt( ids[0] ), Byte.parseByte( ids[1] ) );
								} else {
									respond( sender, plugin.playerError("Invalid block filter '"+val+"'. Use /prism ? [command] for help.") );
									return null;
								}
							} else {
								
								// It's id without a subid
								if(TypeUtils.isNumeric(b)){
									parameters.addBlockFilter( Integer.parseInt(b), (byte)0 );
								} else {
									
									// Lookup the item name, get the ids
									MaterialAliases items = plugin.getItems();
									ArrayList<int[]> itemIds = items.getItemIdsByAlias( b );
									if(itemIds.size() > 0){
										for(int[] ids : itemIds){
											if(ids.length == 2){
												// If we really care about the sub id because it's a whole different item
												if(BlockUtils.hasSubitems(ids[0])){
													parameters.addBlockFilter( ids[0], (byte) ids[1] );
												} else {
													parameters.addBlockFilter( ids[0], (byte)0 );
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
				if(arg_type.equals("before")){
					String date = translateTimeStringToDate(plugin,sender,val);
					if(date != null){
						parameters.setBeforeTime( date );
					} else {
						return null;
					}
				}
				if( arg_type.equals("since") || arg_type.equals("t") ){
					String date = translateTimeStringToDate(plugin,sender,val);
					if(date != null){
						parameters.setSinceTime( date );
					} else {
						return null;
					}
				}
				
				
				// Keyword
				if(arg_type.equals("k")){
					parameters.setKeyword( val );
				}

				
				// Special Flags
				if(arg_type.startsWith("-")){
					try {
						String[] flagComponents = val.substring(1).split("=");
						Flag flag = Flag.valueOf( flagComponents[0].replace("-", "_").toUpperCase() );
						if(!(parameters.hasFlag(flag))){
								
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
					} catch(IllegalArgumentException ex){
						respond( sender, plugin.playerError("Unrecognized flag '"+val+"'. Use /prism ? [command] for help.") );
						return null;
					}
				}
			}
			
			// Validate any required args are set
			if(foundArgs.isEmpty()){
				respond( sender, plugin.playerError("You're missing valid parameters. Use /prism ? for a assitance.") );
				return null;
			}
			
			/**
			 * Enforce defaults, unless we're doing a delete
			 */
			if( !processType.equals(PrismProcessType.DELETE) ){
				// Radius default
				if(!foundArgs.containsKey("r")){
					if(parameters.allowsNoRadius()){
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
				if(!foundArgs.containsKey("t") && !foundArgs.containsKey("before") && !foundArgs.containsKey("since")){
					String date = translateTimeStringToDate(plugin,sender,plugin.getConfig().getString("prism.default-time-since"));
					if(date == null){
						plugin.log("Error - date range configuration for prism.time-since is not valid");
						date = translateTimeStringToDate(plugin,sender,"3d");
					}
					parameters.setSinceTime(date);
					parameters.addDefaultUsed( "t:" + plugin.getConfig().getString("prism.default-time-since") );
				}
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

		Pattern p = Pattern.compile("([0-9]+)(s|h|m|d|w)");
		Calendar cal = Calendar.getInstance();

		String[] matches = TypeUtils.preg_match_all( p, arg_value );
		if(matches.length > 0){
			for(String match : matches){
	
				Matcher m = p.matcher( match );
				if(m.matches()){
					
					if( m.groupCount() == 2 ){
						
						int tfValue = Integer.parseInt( m.group(1) );
						String tfFormat = m.group(2);

						if(tfFormat.equals("w")){
							cal.add(Calendar.WEEK_OF_YEAR, -1 * tfValue);
						}
						else if(tfFormat.equals("d")){
							cal.add(Calendar.DAY_OF_MONTH, -1 * tfValue);
						}
						else if(tfFormat.equals("h")){
							cal.add(Calendar.HOUR, -1 * tfValue);
						}
						else if(tfFormat.equals("m")){
							cal.add(Calendar.MINUTE, -1 * tfValue);
						}
						else if(tfFormat.equals("s")){
							cal.add(Calendar.SECOND, -1 * tfValue);
						} else {
							respond( sender, plugin.playerError("Invalid timeframe values for "+tfFormat+". Use /prism ? for a help.") );
							return null;
						}
					}
				}
			}
			SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			dateFrom = form.format(cal.getTime());
		}
		
		if(dateFrom == null){
			respond( sender, plugin.playerError("Invalid timeframe values. Use /prism ? for a help.") );
		}

		return dateFrom;
		
	}
}
