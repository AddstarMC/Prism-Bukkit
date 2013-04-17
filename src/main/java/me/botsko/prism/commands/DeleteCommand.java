package me.botsko.prism.commands;

import java.util.ArrayList;
import java.util.concurrent.CopyOnWriteArrayList;

import org.bukkit.ChatColor;
import org.bukkit.scheduler.BukkitTask;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.purge.PurgeTask;
import me.botsko.prism.purge.SenderPurgeCallback;

public class DeleteCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	protected BukkitTask deleteTask;
	
	/**
	 * 
	 */
	protected int total_records_affected = 0, cycle_rows_affected = 0;
	
	
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
	public void handle(final CallInfo call){
		
		// Process and validate all of the arguments
		final QueryParameters parameters = PreprocessArgs.process( plugin, call.getSender(), call.getArgs(), PrismProcessType.DELETE, 1 );
		if(parameters == null){
			return;
		}
		parameters.setStringFromRawArgs( call.getArgs(), 1 );
		
		// determine if defaults were used
		ArrayList<String> defaultsUsed = parameters.getDefaultsUsed();
		String defaultsReminder = "";
		if(!defaultsUsed.isEmpty()){
			defaultsReminder += " using defaults:";
			for(String d : defaultsUsed){
				defaultsReminder += " " + d;
			}
		}

		if(parameters.getFoundArgs().size() > 0){
			
			call.getSender().sendMessage( Prism.messenger.playerSubduedHeaderMsg("Purging data..." + defaultsReminder) );
			
			int purge_tick_delay = plugin.getConfig().getInt("prism.purge.batch-tick-delay");
			if(purge_tick_delay < 1){
				purge_tick_delay = 20;
			}
			
			call.getSender().sendMessage( Prism.messenger.playerHeaderMsg("Starting purge cycle." + ChatColor.GRAY + " No one will ever know..."));
			
			// build callback
			SenderPurgeCallback callback = new SenderPurgeCallback();
			callback.setSender( call.getSender() );
			
			// add to an arraylist so we're consistent
			CopyOnWriteArrayList<QueryParameters> paramList = new CopyOnWriteArrayList<QueryParameters>();
			paramList.add( parameters );
			
			Prism.log("Beginning prism database purge cycle. Will be performed in batches so we don't tie up the db...");
			deleteTask = plugin.getServer().getScheduler().runTaskAsynchronously(plugin, new PurgeTask( plugin, paramList, purge_tick_delay, callback ));
			
		} else {
			call.getSender().sendMessage( Prism.messenger.playerError("You must supply at least one parameter." ));
		}
	}
}