package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;

public class DeleteCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public DeleteCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		// If date provided
		if(call.getArgs().length == 2){
			
			String dateBefore = PreprocessArgs.translateTimeStringToDate( plugin, call.getPlayer(), call.getArg(1) );
			if(dateBefore != null && !dateBefore.isEmpty()){
				ActionsQuery aq = new ActionsQuery(plugin);
				int rows_affected = aq.delete(dateBefore);
				call.getPlayer().sendMessage( plugin.playerHeaderMsg( rows_affected + " records have been purged from the database."));
			}
		}
	}
}