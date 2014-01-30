package me.botsko.prism.commandlibs;

import java.util.HashMap;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
		if(processType.equals(PrismProcessType.LOOKUP)){
			parameters.setLimit( plugin.getConfig().getInt("prism.queries.lookup-max-results") );
			parameters.setPerPage( plugin.getConfig().getInt("prism.queries.default-results-per-page") );
		}
		parameters.setProcessType(processType);
		
		// Load registered parameters
		HashMap<Pattern,PrismParameterHandler> registeredParams = Prism.getParameters();
		
		// List all valid handlers
		HashMap<PrismParameterHandler,Matcher> foundArgs = new HashMap<PrismParameterHandler,Matcher>();
		
		if(args != null){
			for (int i = startAt; i < args.length; i++){

				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				boolean handlerFound = false;
				
				// Iterate registered params, check for match
				for( Entry<Pattern,PrismParameterHandler> entry : registeredParams.entrySet() ){
					Matcher m = entry.getKey().matcher(arg);
					if( m.matches() ){
						
						Prism.debug("Parameter regex "+entry.getKey().pattern()+" has " + m.groupCount() + " matches:" );
						for( int z = 0; z < m.groupCount()+1; z++ ){
							Prism.debug("match " + z + ": " + m.group(z));
						}

						if( m.groupCount() < 2 ){
							if( sender != null ) sender.sendMessage( Prism.messenger.playerError("Invalid syntax for parameter " + arg) );
							return null;
						}

						if( m.group(2).isEmpty() ){
							if( sender != null ) sender.sendMessage( Prism.messenger.playerError("Can't use empty values for '"+arg+"'. Use /prism ? for help.") );
							return null;
						}
						
						handlerFound = true;
						
						foundArgs.put( entry.getValue(), m );
						continue;

					} else {
						
						// We support an alternate player syntax so that people can use the tab-complete
						// feature of minecraft. Using p: prevents it.
						Player autoFillPlayer = plugin.getServer().getPlayer(arg);
						if( autoFillPlayer != null ){
							MatchRule match = MatchRule.INCLUDE;
							if(arg.startsWith("!")){
								match = MatchRule.EXCLUDE;
							}
							handlerFound = true;
							parameters.addPlayerName( arg.replace("!", ""), match );
						}
					}
				}
				
				if( !handlerFound ){
					if( sender != null ) sender.sendMessage( Prism.messenger.playerError( "Unrecognized parameter '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
				
			}
			parameters.setFoundArgs( foundArgs );
			
			// Validate any required args are set
			if( foundArgs.isEmpty() ){
				if( sender != null ) sender.sendMessage( Prism.messenger.playerError("You're missing valid parameters. Use /prism ? for assistance.") );
				return null;
			}

			/**
			 * Send arguments to parameter handlers
			 */
			for( Entry<PrismParameterHandler,Matcher> entry : foundArgs.entrySet() ){
				try {
					entry.getKey().process( parameters, entry.getValue(), sender );
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