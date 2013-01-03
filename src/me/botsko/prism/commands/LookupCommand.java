package me.botsko.prism.commands;

import org.bukkit.ChatColor;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Action;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;

public class LookupCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public LookupCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		// Process and validate all of the arguments
		QueryParameters parameters = PreprocessArgs.process( plugin, call.getPlayer(), call.getArgs(), "lookup" );
		if(parameters == null){
			return;
		}
		parameters.setLimit(1000); // @todo config this, and move the logic to queryparams
	
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( call.getPlayer(), parameters );
		if(!results.getActionResults().isEmpty()){
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Showing "+results.getTotal_results()+" results. Page 1 of "+results.getTotal_pages()) );
			for(Action a : results.getPaginatedActionResults()){
				ActionMessage am = new ActionMessage(a);
				if(parameters.getAllow_no_radius()){
					am.hideId(false);
				}
				call.getPlayer().sendMessage( plugin.playerMsg( am.getMessage() ) );
			}
		} else {
			call.getPlayer().sendMessage( plugin.playerError( "Nothing found." + ChatColor.GRAY + " Either you're missing something, or we are." ) );
		}
	}
}