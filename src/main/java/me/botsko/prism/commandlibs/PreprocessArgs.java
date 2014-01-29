package me.botsko.prism.commandlibs;

import java.util.HashMap;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.parameters.PrismParameterHandler;
import me.botsko.prism.utils.DateUtil;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class PreprocessArgs {
	
	
	/**
	 * 
	 * @param args
	 */
	public static QueryParameters process( Prism plugin, CommandSender sender, String[] args, PrismProcessType processType, int startAt, boolean useDefaults ){

		Player player = null;
		if(sender != null && sender instanceof Player){
			player = (Player) sender;
		}
		
		// Define pagination/process type
		QueryParameters parameters = new QueryParameters();
		ConcurrentHashMap<String,String> foundArgs = new ConcurrentHashMap<String,String>();
		if(processType.equals(PrismProcessType.LOOKUP)){
			parameters.setLimit( plugin.getConfig().getInt("prism.queries.lookup-max-results") );
			parameters.setPerPage( plugin.getConfig().getInt("prism.queries.default-results-per-page") );
		}
		parameters.setProcessType(processType);
		
		// Load registered parameters
		HashMap<String,PrismParameterHandler> registeredParams = Prism.getParameters();
		
		if(args != null){
			for (int i = startAt; i < args.length; i++){

				String arg = args[i];
				if (arg.isEmpty()) continue;

				// Split parameter and values
				String[] argEntry = arg.toLowerCase().split(":");
				String arg_type = argEntry[0];
				String val = arg.contains(":") ? arg.replace(argEntry[0] + ":", "") : argEntry[0];
				
				// If the value doesn't match a parameter, see if it's a player
				// allows tab-complete player syntax
				if( !registeredParams.containsKey(arg_type) ){
					
					// We support an alternate player syntax so that people can use the tab-complete
					// feature of minecraft. Using p: prevents it.
					Player autoFillPlayer = plugin.getServer().getPlayer(arg_type);
					if( autoFillPlayer != null ){
						MatchRule match = MatchRule.INCLUDE;
						if(arg_type.startsWith("!")){
							match = MatchRule.EXCLUDE;
						}
						parameters.addPlayerName( arg_type.replace("!", ""), match );
					} else {
						if( sender != null ) sender.sendMessage( Prism.messenger.playerError( "Unrecognized parameter '"+arg+"'. Use /prism ? for help.") );
						return null;
					}
				}
				
				// Verify no empty val
				if(val.isEmpty()){
					if( sender != null ) sender.sendMessage( Prism.messenger.playerError("Can't use empty values for '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				
				// Officially certify we found a valid argument and value!
				if( registeredParams.containsKey(arg_type) ){
					foundArgs.put(arg_type, val);
					parameters.setFoundArgs(foundArgs);
				}
			}
			
			// Validate any required args are set
			if( foundArgs.isEmpty() ){
				if( sender != null ) sender.sendMessage( Prism.messenger.playerError("You're missing valid parameters. Use /prism ? for assistance.") );
				return null;
			}

			/**
			 * Send arguments to parameter handlers
			 */
			for( Entry<String,String> entry : foundArgs.entrySet() ){
				try {
					registeredParams.get( entry.getKey() ).process(parameters, entry.getValue(), sender);
				} catch ( IllegalArgumentException e ){
					if( sender != null ) sender.sendMessage( Prism.messenger.playerError(e.getMessage()) );
					return null;
				}
			}
			
			/**
			 * Enforce defaults, unless we're doing a delete or they've been disabled in the config
			 * @todo We should eventually allow the handler to decide these
			 */
			if( !processType.equals(PrismProcessType.DELETE) && useDefaults ){
				// Radius default, apply only if player present
				if( !foundArgs.containsKey("r") && player != null ){
					if(parameters.allowsNoRadius()){
						// We'll allow no radius.
					} else {
						parameters.setRadius( plugin.getConfig().getInt("prism.queries.default-radius") );
						parameters.addDefaultUsed( "r:" + parameters.getRadius() );
					}
				}
				// World default
				if( player != null && !foundArgs.containsKey("w") && !parameters.allowsNoRadius()){
					parameters.setWorld( player.getWorld().getName() );
				}
				// Time default
				if(!foundArgs.containsKey("t") && !foundArgs.containsKey("before") && !foundArgs.containsKey("since")){
					Long date = DateUtil.translateTimeStringToDate(plugin.getConfig().getString("prism.queries.default-time-since"));
					if(date == 0){
						Prism.log("Error - date range configuration for prism.time-since is not valid");
						date = DateUtil.translateTimeStringToDate("3d");
					}
					parameters.setSinceTime(date);
					parameters.addDefaultUsed( "t:" + plugin.getConfig().getString("prism.queries.default-time-since") );
				}
			}
			
			// Player location
			if( player != null && !plugin.getConfig().getBoolean("prism.queries.never-use-defaults") && parameters.getPlayerLocation() == null ){
				parameters.setMinMaxVectorsFromPlayerLocation( player.getLocation() );
			}
		}
		return parameters;
	}
}