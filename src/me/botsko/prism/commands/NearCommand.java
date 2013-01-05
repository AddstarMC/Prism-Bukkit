package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Action;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.TypeUtils;

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
		
		// allow a custom near radius
		int radius = plugin.getConfig().getInt("prism.near.default-radius");
		if(call.getArgs().length == 2){
			if(TypeUtils.isNumeric(call.getArg(1))){
				int _tmp_radius = Integer.parseInt(call.getArg(1));
				if(_tmp_radius > 0){
					radius = _tmp_radius;
				} else {
					call.getPlayer().sendMessage( plugin.playerError("Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help.") );
					return;
				}
			} else {
				call.getPlayer().sendMessage( plugin.playerError("Radius must be a number. Or leave it off to use the default. Use /prism ? for help.") );
				return;
			}
		}
		
		parameters.setRadius(radius);
		parameters.setLimit(1000); // @todo config this, and move the logic to queryparams
	
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( call.getPlayer(), parameters );
		if(!results.getActionResults().isEmpty()){
			call.getPlayer().sendMessage( plugin.playerSubduedHeaderMsg( "All changes within " + radius + " blocks of you..." ) );
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Showing "+results.getTotal_results()+" results. Page 1 of "+results.getTotal_pages()) );
			for(Action a : results.getPaginatedActionResults()){
				ActionMessage am = new ActionMessage(a);
				if(parameters.getAllow_no_radius()){
					am.hideId(false);
				}
				call.getPlayer().sendMessage( plugin.playerMsg( am.getMessage() ) );
			}
		} else {
			call.getPlayer().sendMessage( plugin.playerError( "Couldn't find anything." ) );
		}
	}
}