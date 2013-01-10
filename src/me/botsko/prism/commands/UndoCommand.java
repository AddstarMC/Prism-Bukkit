package me.botsko.prism.commands;

import java.util.List;

import org.bukkit.ChatColor;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.TypeUtils;

public class UndoCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public UndoCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		if( call.getArgs().length > 1 && TypeUtils.isNumeric(call.getArg(1))){
			int record_id = Integer.parseInt(call.getArg(1));
			if(record_id <= 0){
				call.getPlayer().sendMessage( plugin.playerError( "Record id must be greater than zero." ) );
				return;
			}
		} else {
			
			// Show the list
			// Process and validate all of the arguments
			QueryParameters parameters = new QueryParameters();
			parameters.setAllow_no_radius(true);
			parameters.addActionType(ActionType.PRISM_PROCESS);
			parameters.setPlayer( call.getPlayer().getName() );
			parameters.setLimit(5); // @todo config this, and move the logic to queryparams
		
			ActionsQuery aq = new ActionsQuery(plugin);
			QueryResult results = aq.lookup( call.getPlayer(), parameters );
			if(!results.getActionResults().isEmpty()){
				call.getPlayer().sendMessage( plugin.playerHeaderMsg("Showing "+results.getTotal_results()+" results. Page 1 of "+results.getTotal_pages()) );
				List<Action> paginated = results.getPaginatedActionResults();
				if(paginated != null){
					for(Action a : paginated){
						ActionMessage am = new ActionMessage(a);
						if(parameters.getAllow_no_radius()){
							am.hideId(false);
						}
						call.getPlayer().sendMessage( plugin.playerMsg( am.getMessage() ) );
					}
				} else {
					call.getPlayer().sendMessage( plugin.playerError( "Pagination can't find anything. Do you have the right page number?" ) );
				}
			} else {
				call.getPlayer().sendMessage( plugin.playerError( "Nothing found." + ChatColor.GRAY + " Either you're missing something, or we are." ) );
			}
			
			
		}
	}
}