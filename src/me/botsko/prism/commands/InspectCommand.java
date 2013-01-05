package me.botsko.prism.commands;

import org.bukkit.ChatColor;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class InspectCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public InspectCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		// If already on, turn it off
		if(plugin.playersWithActiveTools.contains(call.getPlayer().getName())){
			plugin.playersWithActiveTools.remove(call.getPlayer().getName());
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Inspection mode " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
		} else {
			plugin.playersWithActiveTools.add(call.getPlayer().getName());
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Inspection mode " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+".") );
		}
	}
}