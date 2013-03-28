package me.botsko.prism.purge;

import java.util.concurrent.CopyOnWriteArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;

public class PurgeTask implements Runnable {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private CopyOnWriteArrayList<QueryParameters> paramList;
	
	/**
	 * 
	 */
	private int cycle_rows_affected = 0;
	
	/**
	 * 
	 */
	private int purge_tick_delay;
	
	/**
	 * 
	 */
	private PurgeCallback callback;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public PurgeTask( Prism plugin, CopyOnWriteArrayList<QueryParameters> paramList, int purge_tick_delay, PurgeCallback callback ){
		this.plugin = plugin;
		this.paramList = paramList;
		this.purge_tick_delay = purge_tick_delay;
		this.callback = callback;
	}
	
	
	/**
	 * 
	 */
	public void run(){
    	if(paramList.size() > 0){
    		
    		boolean cycle_complete = false;
    		
	    	ActionsQuery aq = new ActionsQuery(plugin);
	    	// Execute in batches so we don't tie up the db with one massive query
	    	for( QueryParameters param : paramList ){

				cycle_rows_affected = aq.delete(param);
				plugin.total_records_affected += cycle_rows_affected;
				
				
				
				// If nothing (or less than the limit) has been deleted this cycle, we need to move on
				if( cycle_rows_affected == 0 || cycle_rows_affected < plugin.getConfig().getInt("prism.purge.records-per-batch") ){

					// Remove rule, reset affected count, mark complete
					paramList.remove( param );
					cycle_complete = true;
					
				}
				
				// Send cycle to callback
				callback.cycle( param, cycle_rows_affected, plugin.total_records_affected, cycle_complete );
				
				// If cycle is incomplete, reschedule it, or reset counts
				if( !cycle_complete ){
					plugin.deleteTask = plugin.getServer().getScheduler().runTaskLaterAsynchronously(plugin, new PurgeTask( plugin, paramList, purge_tick_delay, callback ), purge_tick_delay);
				} else {
					plugin.total_records_affected = 0;
				}
	    	}
    	}
	}
}