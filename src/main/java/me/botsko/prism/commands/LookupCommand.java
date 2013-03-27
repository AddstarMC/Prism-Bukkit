package me.botsko.prism.commands;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.ChatColor;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Flag;
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
		QueryParameters parameters = PreprocessArgs.process( plugin, call.getSender(), call.getArgs(), PrismProcessType.LOOKUP, 1 );
		if(parameters == null){
			return;
		}
		
		// determine if defaults were used
		ArrayList<String> defaultsUsed = parameters.getDefaultsUsed();
		String defaultsReminder = "";
		if(!defaultsUsed.isEmpty()){
			defaultsReminder += "Using defaults:";
			for(String d : defaultsUsed){
				defaultsReminder += " " + d;
			}
		}
	
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( parameters, call.getSender() );
		if(!results.getActionResults().isEmpty()){
			call.getSender().sendMessage( Prism.messenger.playerHeaderMsg("Showing "+results.getTotal_results()+" results. Page 1 of "+results.getTotal_pages()) );
			if(!defaultsReminder.isEmpty()){
				call.getSender().sendMessage( Prism.messenger.playerSubduedHeaderMsg(defaultsReminder) );
			}
			List<Handler> paginated = results.getPaginatedActionResults();
			if(paginated != null){
				for(Handler a : paginated){
					ActionMessage am = new ActionMessage(a);
					if( parameters.allowsNoRadius() || parameters.hasFlag(Flag.EXTENDED) || plugin.getConfig().getBoolean("prism.messenger.always-show-extended") ){
						am.showExtended();
					}
					call.getSender().sendMessage( Prism.messenger.playerMsg( am.getMessage() ) );
				}
			} else {
				call.getSender().sendMessage( Prism.messenger.playerError( "Pagination can't find anything. Do you have the right page number?" ) );
			}
		} else {
			if(!defaultsReminder.isEmpty()){
				call.getSender().sendMessage( Prism.messenger.playerSubduedHeaderMsg(defaultsReminder) );
			}
			call.getSender().sendMessage( Prism.messenger.playerError( "Nothing found." + ChatColor.GRAY + " Either you're missing something, or we are." ) );
		}
	}
}