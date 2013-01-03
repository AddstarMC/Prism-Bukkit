package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Action;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class NearCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public NearCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {

		// Build params
		QueryParameters parameters = new QueryParameters();
		parameters.setWorld( call.getPlayer().getWorld().getName() );
		parameters.setPlayer_location(call.getPlayer().getLocation().toVector());
		
		parameters.setRadius(5); // @todo config this
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
			call.getPlayer().sendMessage( plugin.playerError( "Couldn't find any nearby changes." ) );
		}
	}
}