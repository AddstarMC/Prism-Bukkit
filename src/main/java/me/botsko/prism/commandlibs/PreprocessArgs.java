package me.botsko.prism.commandlibs;

import java.util.HashMap;
import java.util.Map.Entry;
import java.util.regex.Matcher;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.parameters.PrismParameterHandler;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class PreprocessArgs {
	
	
	
	/**
	 * 
	 * @param args
	 */
	public static QueryParameters process( Prism plugin, CommandSender sender, String[] args, PrismProcessType processType, int startAt, boolean useDefaults ){

		// Check for player or sender
		Player player = null;
		if(sender != null && sender instanceof Player){
			player = (Player) sender;
		}
		
		// Start query
		QueryParameters parameters = new QueryParameters();
		parameters.setProcessType(processType);
		
		// Define pagination/process type
		if(parameters.getProcessType().equals(PrismProcessType.LOOKUP)){
			parameters.setLimit( plugin.getConfig().getInt("prism.queries.lookup-max-results") );
			parameters.setPerPage( plugin.getConfig().getInt("prism.queries.default-results-per-page") );
		}
		
		// Load registered parameters
		HashMap<String,PrismParameterHandler> registeredParams = Prism.getParameters();
		
		// Store names of matched params/handlers
		HashMap<String,Matcher> foundArgs = new HashMap<String,Matcher>();
		
		// Iterate all command arguments
		if(args != null){
			for (int i = startAt; i < args.length; i++){

				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				boolean handlerFound = false;
				
				// Match command argument to parameter handler
				for( Entry<String,PrismParameterHandler> entry : registeredParams.entrySet() ){
					Matcher m = entry.getValue().getArgumentPattern().matcher(arg);
					if( m.matches() ){
						
						Prism.debug("Parameter regex "+entry.getValue().getArgumentPattern().pattern()+" has " + m.groupCount() + " matches:" );
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
						
						foundArgs.put( entry.getValue().getName().toLowerCase(), m );
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
				
				// Reject argument that doesn't match anything
				if( !handlerFound ){
					if( sender != null ) sender.sendMessage( Prism.messenger.playerError( "Unrecognized parameter '"+arg+"'. Use /prism ? for help.") );
					return null;
				}
			}
			parameters.setFoundArgs( foundArgs );
			
			// Reject no matches
			if( foundArgs.isEmpty() ){
				if( sender != null ) sender.sendMessage( Prism.messenger.playerError("You're missing valid parameters. Use /prism ? for assistance.") );
				return null;
			}

			/**
			 * Call default method for handlers *not* used
			 */
			if( useDefaults ){
				for( Entry<String,PrismParameterHandler> entry : registeredParams.entrySet() ){
					if( !foundArgs.containsKey(entry.getKey()) ){
						entry.getValue().defaultTo(parameters, sender);
					}
				}
			}

			/**
			 * Send arguments to parameter handlers
			 */
			for( Entry<String,Matcher> entry : foundArgs.entrySet() ){
				try {
					PrismParameterHandler handler = registeredParams.get(entry.getKey());
					handler.process( parameters, entry.getValue(), sender );
				} catch ( IllegalArgumentException e ){
					if( sender != null ) sender.sendMessage( Prism.messenger.playerError(e.getMessage()) );
					return null;
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