package me.botsko.prism.commands;

import java.util.ArrayList;

import org.bukkit.ChatColor;
import org.bukkit.scheduler.BukkitTask;

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
		
		call.getSender().sendMessage( Prism.messenger.playerSubduedHeaderMsg("Purging data..." + defaultsReminder) );
			
		if(parameters.getFoundArgs().size() > 0){
			
			int purge_tick_delay = plugin.getConfig().getInt("prism.purge.batch-tick-delay");
			if(purge_tick_delay < 1){
				purge_tick_delay = 20;
			}
			
			call.getSender().sendMessage( Prism.messenger.playerHeaderMsg("Starting purge cycle." + ChatColor.GRAY + " No one will ever know..."));
			
			plugin.log("Beginning prism database purge cycle. Will be performed in batches so we don't tie up the db...");
			deleteTask = plugin.getServer().getScheduler().runTaskTimerAsynchronously(plugin, new Runnable(){
			    public void run(){
		
					ActionsQuery aq = new ActionsQuery(plugin);
					// Execute in batches so we don't tie up the db with one massive query

					cycle_rows_affected = aq.delete(parameters);
					plugin.debug("Purge cycle cleared " + cycle_rows_affected + " rows.");
					total_records_affected += cycle_rows_affected;
					
					if( cycle_rows_affected == 0 || cycle_rows_affected <= plugin.getConfig().getInt("prism.purge.records-per-batch") ){
						deleteTask.cancel();
						plugin.log("Cleared " + total_records_affected + " rows from the database. Using:" + parameters.getOriginalCommand() );
						call.getSender().sendMessage( Prism.messenger.playerHeaderMsg(total_records_affected + " records have been purged."));
						total_records_affected = 0;
					} else {
						call.getSender().sendMessage( Prism.messenger.playerSubduedHeaderMsg("Purge cycle cleared " + cycle_rows_affected + " records."));
					}
			    }
			}, 0, purge_tick_delay);
		} else {
			call.getSender().sendMessage( Prism.messenger.playerError("You must supply at least one parameter." ));
		}
	}
}