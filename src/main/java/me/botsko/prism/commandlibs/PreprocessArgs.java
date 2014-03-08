package me.botsko.prism.commandlibs;

import java.util.*;
import java.util.Map.Entry;

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
		Set<String> foundArgsNames = new HashSet<String>();
		List<MatchedParam> foundArgsList = new ArrayList<MatchedParam>();
		
		// Iterate all command arguments
		if(args != null){
			for (int i = startAt; i < args.length; i++){

				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				boolean handlerFound = false;
				
				// Match command argument to parameter handler
				for( Entry<String,PrismParameterHandler> entry : registeredParams.entrySet() ){
					if(entry.getValue().applicable(parameters, arg, sender)) {
						handlerFound = true;
						foundArgsList.add(new MatchedParam(entry.getValue(), arg));
						foundArgsNames.add(entry.getValue().getName());
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
			parameters.setFoundArgs( foundArgsNames );
			
			// Reject no matches
			if( foundArgsList.isEmpty() ){
				if( sender != null ) sender.sendMessage( Prism.messenger.playerError("You're missing valid parameters. Use /prism ? for assistance.") );
				return null;
			}

			/**
			 * Call default method for handlers *not* used
			 */
			if( useDefaults ){
				for( Entry<String,PrismParameterHandler> entry : registeredParams.entrySet() ){
					if( !foundArgsNames.contains(entry.getKey()) ){
						entry.getValue().defaultTo(parameters, sender);
					}
				}
			}

			/**
			 * Send arguments to parameter handlers
			 */
			for (MatchedParam matchedParam : foundArgsList) {
				try {
					PrismParameterHandler handler = matchedParam.getHandler();
					handler.process( parameters, matchedParam.getArg(), sender );
				} catch ( IllegalArgumentException e ){
					if( sender != null ) sender.sendMessage( Prism.messenger.playerError(e.getMessage()) );
					return null;
				}
			}
			
			// Player location
			if( player != null && !plugin.getConfig().getBoolean("prism.queries.never-use-defaults") && parameters.getPlayerLocation() == null && (parameters.getMaxLocation() == null || parameters.getMinLocation() == null) ){
				parameters.setMinMaxVectorsFromPlayerLocation( player.getLocation() );
			}
		}
		return parameters;
	}

	
	/**
	 * 
	 */
	private static class MatchedParam {
		private final PrismParameterHandler handler;
		private final String arg;

		public MatchedParam(PrismParameterHandler handler, String arg) {
			this.handler = handler;
			this.arg = arg;
		}

		public PrismParameterHandler getHandler() {
			return handler;
		}

		public String getArg() {
			return arg;
		}
	}
}