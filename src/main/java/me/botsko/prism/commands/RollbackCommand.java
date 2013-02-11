package me.botsko.prism.commands;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.appliers.Rollback;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;

public class RollbackCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public RollbackCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		QueryParameters parameters = PreprocessArgs.process( plugin, call.getPlayer(), call.getArgs(), PrismProcessType.ROLLBACK, 1 );
		if(parameters == null){
			return;
		}
		parameters.setStringFromRawArgs( call.getArgs(), 1 );
		
		// determine if defaults were used
		ArrayList<String> defaultsUsed = parameters.getDefaultsUsed();
		String defaultsReminder = "";
		if(!defaultsUsed.isEmpty()){
			defaultsReminder += " using defaults:";
			for(String d : defaultsUsed){
				defaultsReminder += " " + d;
			}
		}
		
		call.getPlayer().sendMessage( plugin.playerSubduedHeaderMsg("Preparing results..." + defaultsReminder) );
	
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( parameters, call.getPlayer() );
		if(!results.getActionResults().isEmpty()){
			
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Beginning rollback...") );
			
			Rollback rb = new Rollback( plugin, call.getPlayer(), PrismProcessType.ROLLBACK, results.getActionResults(), parameters );
			rb.apply();
			
		} else {
			call.getPlayer().sendMessage( plugin.playerError( "Nothing found to rollback. Try using /prism l (args) first." ) );
		}
	}
}