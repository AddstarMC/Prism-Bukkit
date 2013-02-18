package me.botsko.prism.commands;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
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
	public void handle(final CallInfo call) {
		
		// Process and validate all of the arguments
		final QueryParameters parameters = PreprocessArgs.process( plugin, call.getSender(), call.getArgs(), PrismProcessType.DELETE, 1 );
		if(parameters == null){
			return;
		}
		parameters.setStringFromRawArgs( call.getArgs() );
		
		// determine if defaults were used
		ArrayList<String> defaultsUsed = parameters.getDefaultsUsed();
		String defaultsReminder = "";
		if(!defaultsUsed.isEmpty()){
			defaultsReminder += " using defaults:";
			for(String d : defaultsUsed){
				defaultsReminder += " " + d;
			}
		}
		
		call.getSender().sendMessage( plugin.messenger.playerSubduedHeaderMsg("Purging data..." + defaultsReminder) );
			
		if(parameters.getFoundArgs().size() > 0){
			plugin.getServer().getScheduler().runTaskAsynchronously(plugin, new Runnable(){
			    public void run(){
					ActionsQuery aq = new ActionsQuery(plugin);
					int rows_affected = aq.delete(parameters);
					call.getSender().sendMessage( plugin.messenger.playerHeaderMsg(rows_affected + " records have been purged from the database."));
			    }
			});
		} else {
			call.getSender().sendMessage( plugin.messenger.playerError("You must supply at least one parameter." ));
		}
	}
}