package me.botsko.prism.commands;

import java.util.Calendar;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.PrismProcessType;
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
		
		Calendar lCDateTime = Calendar.getInstance();
		long processStartTime = lCDateTime.getTimeInMillis();
		
		QueryParameters parameters = PreprocessArgs.process( plugin, call.getPlayer(), call.getArgs(), PrismProcessType.RESTORE, 1 );
		if(parameters == null){
			return;
		}
		parameters.setStringFromRawArgs( call.getArgs() );
	
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( call.getPlayer(), parameters );
		if(!results.getActionResults().isEmpty()){
			
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Restoring changes...") );

			// Inform nearby players
			plugin.notifyNearby(call.getPlayer(), parameters.getRadius(), call.getPlayer().getDisplayName() + " is re-applying block changes nearby. Just so you know.");
			
			// Perform restore
			Restore rs = new Restore( plugin, call.getPlayer(), PrismProcessType.RESTORE, results.getActionResults(), parameters, processStartTime );
			rs.apply();
			
		} else {
			call.getPlayer().sendMessage( plugin.playerError( "Nothing found to restore. Try using /prism l (args) first." ) );
		}
	}
}