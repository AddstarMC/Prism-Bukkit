package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.Preview;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;

public class PreviewCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public PreviewCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		// Cancel or Apply
		if( call.getArgs().length == 2 ){
			
			// Apply
			if(call.getArg(1).equalsIgnoreCase("apply") ){
				Preview pv = new Preview( plugin, call.getPlayer(), null );
				pv.apply_preview();
				return;
			}
			
			// Cancel
			if(call.getArg(1).equalsIgnoreCase("cancel") ){
				Preview pv = new Preview( plugin, call.getPlayer(), null );
				pv.cancel_preview();
				return;
			}
		}
		
		
		// Ensure user has no current preview
		if(plugin.playerActivePreviews.containsKey(call.getPlayer().getName())){
			call.getPlayer().sendMessage( plugin.playerError("You have an existing preview pending. Please apply or cancel before moving on.") );
			return;
		}
		
		QueryParameters parameters = PreprocessArgs.process( plugin, call.getPlayer(), call.getArgs(), "rollback" );
		if(parameters == null){
			return;
		}
	
		// Perform preview
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( call.getPlayer(), parameters );
		if(!results.getActionResults().isEmpty()){
			
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Beginning rollback preview...") );
			
			Preview pv = new Preview( plugin, call.getPlayer(), results.getActionResults() );
			pv.preview( parameters );
			
		} else {
			// @todo no results
		}
	}
}