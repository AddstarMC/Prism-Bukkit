package me.botsko.prism.commands;

import org.bukkit.ChatColor;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class AboutCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public AboutCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		call.getSender().sendMessage( Prism.messenger.playerHeaderMsg("Prism - By viveleroi." + ChatColor.GRAY + " Version: " + plugin.getPrismVersion() ) );
		call.getSender().sendMessage( Prism.messenger.playerSubduedHeaderMsg("IRC: irc.esper.net #prism") );
		call.getSender().sendMessage( Prism.messenger.playerSubduedHeaderMsg("Site/Wiki: http://discover-prism.com") );
	}
}