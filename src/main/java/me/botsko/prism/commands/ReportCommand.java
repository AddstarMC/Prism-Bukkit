package me.botsko.prism.commands;

import java.text.SimpleDateFormat;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentSkipListMap;

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

		if (call.getArgs().length != 2) {
			call.getSender().sendMessage(Prism.messenger.playerError("Please specify a report. Use /prism ? for help."));
			return;
		}

		// /prism report queue
		if (call.getArg(1).equals("queue")) {
			queueReport(call.getSender());
		}
	}

	/**
	 * @param sender
	 */
	protected void queueReport(CommandSender sender) {

		sender.sendMessage(Prism.messenger.playerHeaderMsg("Current Stats"));

		sender.sendMessage(Prism.messenger.playerMsg("Actions in save queue: " + ChatColor.WHITE + Prism.actionsRecorder.getQueueSize()));

		ConcurrentSkipListMap<Long, Integer> runs = plugin.queueStats.getRecentRunCounts();
		if (runs.size() > 0) {
			sender.sendMessage(Prism.messenger.playerHeaderMsg("Recent queue save stats:"));
			for (Entry<Long, Integer> entry : runs.entrySet()) {
				String time = new SimpleDateFormat("HH:mm:ss").format(entry.getKey());
				sender.sendMessage(Prism.messenger.playerMsg(ChatColor.GRAY + time + " " + ChatColor.WHITE + entry.getValue()));
			}
		}
	}
}
