package me.botsko.prism.commands;

import com.sk89q.worldedit.IncompleteRegionException;
import com.sk89q.worldedit.LocalPlayer;
import com.sk89q.worldedit.LocalWorld;
import com.sk89q.worldedit.Vector;
import com.sk89q.worldedit.bukkit.BukkitPlayer;
import com.sk89q.worldedit.regions.Region;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class WorldEditCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public WorldEditCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		if (plugin.plugin_worldEdit == null) {
			call.getPlayer().sendMessage( plugin.playerError("This feature is disabled because Prism couldn't find WorldEdit.") );
			return;
		}
		
		// Get selected area
		Region region = null;
		try {
			LocalPlayer lp = new BukkitPlayer(plugin.plugin_worldEdit, plugin.plugin_worldEdit.getWorldEdit().getServer(), call.getPlayer());
			LocalWorld lw = lp.getWorld();
			region = plugin.plugin_worldEdit.getWorldEdit().getSession(lp).getSelection(lw);
		} catch (IncompleteRegionException e) {
			call.getPlayer().sendMessage( plugin.playerError("You must have a complete WorldEdit selection before using this feature.") );
			return;
		}
		
		//Set WorldEdit locations
		Vector minLoc = new Vector(region.getMinimumPoint().getX(), region.getMinimumPoint().getY(), region.getMinimumPoint().getZ());
		Vector maxLoc = new Vector(region.getMaximumPoint().getX(), region.getMaximumPoint().getY(), region.getMaximumPoint().getZ());
		
//		Calendar lCDateTime = Calendar.getInstance();
//		long processStartTime = lCDateTime.getTimeInMillis();
//		
//		QueryParameters parameters = PreprocessArgs.process( plugin, call.getPlayer(), call.getArgs(), PrismProcessType.ROLLBACK, 1 );
//		if(parameters == null){
//			return;
//		}
//		parameters.setStringFromRawArgs( call.getArgs() );
//		
//		call.getPlayer().sendMessage( plugin.playerSubduedHeaderMsg("Preparing results...") );
//	
//		ActionsQuery aq = new ActionsQuery(plugin);
//		QueryResult results = aq.lookup( call.getPlayer(), parameters );
//		if(!results.getActionResults().isEmpty()){
//			
//			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Beginning rollback...") );
//			
//			Rollback rb = new Rollback( plugin, call.getPlayer(), PrismProcessType.ROLLBACK, results.getActionResults(), parameters, processStartTime );
//			rb.apply();
//			
//		} else {
//			call.getPlayer().sendMessage( plugin.playerError( "Nothing found to rollback. Try using /prism l (args) first." ) );
//		}
	}
}