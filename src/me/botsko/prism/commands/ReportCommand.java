package me.botsko.prism.commands;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class ReportCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public ReportCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		if(call.getArgs().length != 2){
			call.getPlayer().sendMessage( plugin.playerError( "Please specify a report. Use /prism ? for help." ) );
			return;
		}
		
		// /prism report queue
		if(call.getArg(1).equals("queue")){
			queueReport( call.getSender() );
		}
	}
	
	
	/**
	 * 
	 * @param sender
	 */
	protected void queueReport( CommandSender sender ){
		sender.sendMessage( plugin.playerHeaderMsg( "Actions in save queue: " + ChatColor.WHITE + Prism.actionsRecorder.getQueueSize() ) );
		sender.sendMessage( plugin.playerHeaderMsg( "Recorded since last run: " + ChatColor.WHITE + plugin.queueStats.actionsRecorded ) );
	}
}