package me.botsko.prism.commands;

import org.bukkit.Location;
import org.bukkit.World;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.TypeUtils;

public class TeleportCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public TeleportCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		if(!TypeUtils.isNumeric(call.getArg(1))){
			call.getPlayer().sendMessage( Prism.messenger.playerError("You must provide a numeric record ID to teleport to." ) );
			return;
		}
			
		int record_id = Integer.parseInt(call.getArg(1));
		if(record_id <= 0){
			call.getPlayer().sendMessage( Prism.messenger.playerError("Record id must be greater than zero." ) );
			return;
		}
			
		// Build params
		QueryParameters params = new QueryParameters();
		params.setWorld( call.getPlayer().getWorld().getName() );
		params.setId(record_id);

		// Query
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( params, call.getPlayer() );
		if(results.getActionResults().isEmpty()){
			call.getPlayer().sendMessage( Prism.messenger.playerError("No records exists with this ID." ) );
			return;
		}
		
		
		for(me.botsko.prism.actions.Action a : results.getActionResults()){
			
			World world = plugin.getServer().getWorld( a.getWorldName() );
			if(world == null){
				call.getPlayer().sendMessage( Prism.messenger.playerError("Action record occurred in world we can't find anymore." ) );
				return;
			}
			
			Location loc = new Location(world,a.getX(),a.getY(),a.getZ());
//			if(Teleport.isSafe(world, loc)){
			call.getPlayer().teleport(loc);
//			} else {
//				call.getPlayer().sendMessage( plugin.playerError( "Doesn't appear to be safe to teleport there." ) );
//			}
		}
	}
}