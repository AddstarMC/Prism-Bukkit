package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.Restore;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;

public class RestoreCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public RestoreCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		QueryParameters parameters = PreprocessArgs.process( plugin, call.getPlayer(), call.getArgs(), "rollback" );
		if(parameters == null){
			return;
		}
	
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( call.getPlayer(), parameters );
		if(!results.getActionResults().isEmpty()){
			
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Restoring changes...") );

			// Inform nearby players
			plugin.notifyNearby(call.getPlayer(), parameters.getRadius(), call.getPlayer().getDisplayName() + " is re-applying block changes nearby. Just so you know.");
			
			// Perform restore
			Restore rs = new Restore( plugin, results.getActionResults() );
			String response = rs.restore();
			if(response != null){
				call.getPlayer().sendMessage(response);
			}
			
		} else {
			// @todo no results
		}
	}
}