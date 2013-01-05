package me.botsko.prism.commands;

import org.bukkit.ChatColor;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.wands.RollbackWand;
import me.botsko.prism.wands.Wand;

public class WandCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public WandCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		if(plugin.playersWithActiveTools.containsKey(call.getPlayer().getName())){
			// Pull the wand in use
			Wand wand = plugin.playersWithActiveTools.get(call.getPlayer().getName());
			if(wand != null){
				if( wand instanceof RollbackWand){
					plugin.playersWithActiveTools.remove(call.getPlayer().getName());
					call.getPlayer().sendMessage( plugin.playerHeaderMsg("Rollback wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
				} else {
					call.getPlayer().sendMessage( plugin.playerError("You have another wand active, please disable it first.") );
				}
			}
		} else {
			RollbackWand wand = new RollbackWand( plugin );
			plugin.playersWithActiveTools.put(call.getPlayer().getName(), wand);
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Rollback wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+".") );
		}
	}
}