package me.botsko.prism.commandlibs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.utils.TypeUtils;

import org.bukkit.Material;
import org.bukkit.entity.Player;

public class PreprocessArgs {
	
	
	/**
	 * 
	 * @param args
	 */
	public static QueryParameters process( Prism plugin, Player player, String[] args, String lookup_type, int starting ){
		
		QueryParameters parameters = new QueryParameters();
		HashMap<String,String> foundArgs = new HashMap<String,String>();
		
		parameters.setLookup_type(lookup_type);
		
		if(args != null){
		
			// Iterate over arguments
			for (int i = starting; i < args.length; i++) {
				
				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				// Verify they're formatting like a:[val]
				if(!arg.contains(":")){
					player.sendMessage( plugin.playerError("Parameter format error for '"+arg+"'. Use /prism ? for a assitance.") );
					return null;
				}
				if (!arg.substring(1,2).equals(":")) {
					player.sendMessage( plugin.playerError("Misplaced ':' (colon) error for '"+arg+"'. Use /prism ? for a assitance.") );
					return null;
				}
				
				// Split parameter and values
				String arg_type = arg.toLowerCase().substring(0,1);
				String val = arg.toLowerCase().substring(2);
				String[] possibleArgs = {"a","r","t","p","w","b","e"};
				if(Arrays.asList(possibleArgs).contains(arg_type)){
					if(!val.isEmpty()){
						plugin.debug("Found arg type " + arg_type + " with value: " + val);
						foundArgs.put(arg_type, val);
						parameters.setFoundArgs(foundArgs);
					} else {
						player.sendMessage( plugin.playerError("You must supply at least one parameter value. Use /prism ? for a assitance.") );
						return null;
					}
				}
				
				// Action
				if(arg_type.equals("a")){
					String[] actions = val.split(",");
					if(actions.length > 0){
						for(String action : actions){
							ActionType actionType = ActionType.getByActionType( action );
							if(actionType != null){
								parameters.addActionType( actionType );
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
						if(radius > plugin.getConfig().getInt("prism.max-radius-unless-overridden")){
							radius = plugin.getConfig().getInt("prism.max-radius-unless-overridden");
							player.sendMessage( plugin.playerError("Forcing radius to " + radius + " as allowed by config.") );
						}
						parameters.setRadius( radius );
					} else {
						// User has asked for a global radiu
						if(val.equals("global") && parameters.getLookup_type().equals("lookup")){
							parameters.setAllow_no_radius(true);
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
						
						ArrayList<String> _tmp_vals = new ArrayList<String>();
						
						for(String b : blocks){
					
							// if user provided id:subid
							if(b.contains(":") && b.length() >= 3){
								String _tmp_id = b.substring(0,1);
								String _tmp_subid = b.substring(2);
								if(!TypeUtils.isNumeric(_tmp_id) || !TypeUtils.isNumeric(_tmp_subid)){
									_tmp_vals.add(_tmp_id+":"+_tmp_subid);
								}
							} else {
								
								// It's id without a subid
								if(TypeUtils.isNumeric(b)){
									_tmp_vals.add(b+":0");
								} else {
									
									// Are they using a block name?
									// @todo we need better names. defaults suck
									Material m = Material.getMaterial( b.toUpperCase() );
									if(m != null){
										_tmp_vals.add(m.getId()+":0");
									}
								}
							}
						}
						if(_tmp_vals.size() > 0){
							parameters.setBlock( TypeUtils.join(_tmp_vals, ",") );
						}
					}
				}
				
				// Time
				if(arg_type.equals("t")){
					parameters.setTime( val );
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
					plugin.debug("Setting default radius to " + plugin.getConfig().getInt("default-radius"));
					parameters.setRadius( plugin.getConfig().getInt("prism.default-radius") );
				}
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

}
