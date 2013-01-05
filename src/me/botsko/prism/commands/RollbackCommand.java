package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.ApplierResult;
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
		
		QueryParameters parameters = PreprocessArgs.process( plugin, call.getPlayer(), call.getArgs(), "rollback" );
		if(parameters == null){
			return;
		}
	
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( call.getPlayer(), parameters );
		if(!results.getActionResults().isEmpty()){
			
			// Inform nearby players
			plugin.notifyNearby(call.getPlayer(), parameters.getRadius(), call.getPlayer().getDisplayName() + " is performing a rollback nearby. Just so you know.");
			
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Beginning rollback...") );
			Rollback rb = new Rollback( plugin, call.getPlayer(), results.getActionResults(), parameters );
			ApplierResult result = rb.apply();
			if(!result.getMessages().isEmpty()){
				for(String resp : result.getMessages()){
					call.getPlayer().sendMessage(resp);
				}
			}
		} else {
			// @todo no results
		}
	}
}