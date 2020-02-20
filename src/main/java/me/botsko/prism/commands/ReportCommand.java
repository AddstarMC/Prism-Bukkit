package me.botsko.prism.commands;

import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.pool.HikariPool;
import me.botsko.prism.database.ActionReportQuery;
import me.botsko.prism.database.BlockReportQuery;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.RecordingManager;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.MiscUtils;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.scheduler.BukkitScheduler;

import java.sql.Connection;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentSkipListMap;

public class ReportCommand implements SubHandler {

	/**
	 * 
	 */
	private final Prism plugin;
	private final List<String> secondaries, sumTertiaries;

	/**
	 * 
	 * @param plugin
	 * @return
	 */
	public ReportCommand(Prism plugin) {
		this.plugin = plugin;
		secondaries = new ArrayList<>();
		secondaries.add("queue");
		secondaries.add("db");
		secondaries.add("sum");
		sumTertiaries = new ArrayList<>();
		sumTertiaries.add("blocks");
		sumTertiaries.add("actions");
	}

	/**
	 * Handle the command
	 */
	@Override
	public void handle(CallInfo call) {

		if (call.getArgs().length < 2) {
			call.getSender()
					.sendMessage(Prism.messenger.playerError("Please specify a report. Use /prism ? for help."));
			return;
		}

		// /prism report queue
		if (call.getArg(1).equals("queue")) {
			queueReport(call.getSender());
		}

		// /prism report db
		if (call.getArg(1).equals("db")) {
			databaseReport(call.getSender());
		}

		// /prism report queue
		if (call.getArg(1).equals("sum")) {

			if (call.getArgs().length < 3) {
				call.getSender().sendMessage(
						Prism.messenger.playerError("Please specify a 'sum' report. Use /prism ? for help."));
				return;
			}

			if (call.getArgs().length < 4) {
				call.getSender().sendMessage(
						Prism.messenger.playerError("Please provide a player name. Use /prism ? for help."));
				return;
			}

			if (call.getArg(2).equals("blocks")) {
				blockSumReports(call);
			}

			if (call.getArg(2).equals("actions")) {
				actionTypeCountReport(call);
			}
		}
	}

	@Override
	public List<String> handleComplete(CallInfo call) {
		if (call.getArgs().length == 2) {
			return MiscUtils.getStartingWith(call.getArg(1), secondaries);
		}
		if (call.getArg(1).equals("sum")) {
			if (call.getArgs().length == 3) {
				return MiscUtils.getStartingWith(call.getArg(2), sumTertiaries);
			}
			return PreprocessArgs.complete(call.getSender(), call.getArgs());
		}
		return null;
	}

	/**
	 * 
	 * @param sender
	 */
	protected void queueReport(CommandSender sender) {

		sender.sendMessage(Prism.messenger.playerHeaderMsg("Current Stats"));

		sender.sendMessage(
				Prism.messenger.playerMsg("Actions in queue: " + ChatColor.WHITE + RecordingQueue.getQueueSize()));

		final ConcurrentSkipListMap<Long, Integer> runs = plugin.queueStats.getRecentRunCounts();
		if (runs.size() > 0) {
			sender.sendMessage(Prism.messenger.playerHeaderMsg("Recent queue save stats:"));
			for (final Entry<Long, Integer> entry : runs.entrySet()) {
				final String time = new SimpleDateFormat("HH:mm:ss").format(entry.getKey());
				sender.sendMessage(
						Prism.messenger.playerMsg(ChatColor.GRAY + time + " " + ChatColor.WHITE + entry.getValue()));
			}
		}
	}

	/**
	 * 
	 * @param sender
	 */
	protected void databaseReport(CommandSender sender) {

		sender.sendMessage(Prism.messenger.playerHeaderMsg("Database Connection State"));

		sender.sendMessage(Prism.messenger
				.playerMsg("Active Failure Count: " + ChatColor.WHITE + RecordingManager.failedDbConnectionCount));
		sender.sendMessage(
				Prism.messenger.playerMsg("Actions in queue: " + ChatColor.WHITE + RecordingQueue.getQueueSize()));

		if(Prism.getPrismDataSource().getDataSource() instanceof HikariDataSource) {
			HikariDataSource ds = (HikariDataSource) Prism.getPrismDataSource().getDataSource();

			sender.sendMessage(Prism.messenger.playerMsg("Pool total: " + ChatColor.WHITE
					+ ds.getHikariPoolMXBean().getTotalConnections()));
			sender.sendMessage(Prism.messenger.playerMsg("Pool active: " + ChatColor.WHITE
					+ ds.getHikariPoolMXBean().getActiveConnections()));
			sender.sendMessage(Prism.messenger.playerMsg("Pool idle: " + ChatColor.WHITE
					+ ds.getHikariPoolMXBean().getIdleConnections()));
			sender.sendMessage(Prism.messenger.playerMsg("Pool min idle: " + ChatColor.WHITE
					+ ds.getMinimumIdle()));
			sender.sendMessage(Prism.messenger.playerMsg("Pool max idle: " + ChatColor.WHITE
					+ ds.getMaximumPoolSize()));
		}

		boolean recorderActive = false;
		if (plugin.recordingTask != null) {
			final int taskId = plugin.recordingTask.getTaskId();
			final BukkitScheduler scheduler = Bukkit.getScheduler();
			if (scheduler.isCurrentlyRunning(taskId) || scheduler.isQueued(taskId)) {
				recorderActive = true;
			}
		}

		if (recorderActive) {
			sender.sendMessage(Prism.messenger.playerSuccess("Recorder is currently queued or running!"));
		}
		else {
			sender.sendMessage(
					Prism.messenger.playerError("Recorder stopped running! DB conn problems? Try /pr recorder start"));
		}

		sender.sendMessage(Prism.messenger.playerSubduedHeaderMsg("Attempting to check connection readiness..."));

		Connection conn = null;
		try {

			conn = Prism.getPrismDataSource().getConnection();
			if (conn == null) {
				sender.sendMessage(Prism.messenger.playerError("Pool returned NULL instead of a valid connection."));
			}
			else if (conn.isClosed()) {
				sender.sendMessage(Prism.messenger.playerError("Pool returned an already closed connection."));
			}
			else if (conn.isValid(5)) {
				sender.sendMessage(Prism.messenger.playerSuccess("Pool returned valid connection!"));
			}
		}
		catch (final SQLException e) {
			sender.sendMessage(Prism.messenger.playerError("Error: " + e.getMessage()));
			e.printStackTrace();
		}
		finally {
			if (conn != null)
				try {
					conn.close();
				}
				catch (final SQLException ignored) {
				}
		}
	}

	/**
	 * 
	 * @param call Sender
	 */
	protected void blockSumReports(final CallInfo call) {

		// Process and validate all of the arguments
		final QueryParameters parameters = PreprocessArgs.process(plugin, call.getSender(), call.getArgs(),
				PrismProcessType.LOOKUP, 3, !plugin.getConfig().getBoolean("prism.queries.never-use-defaults"));
		if (parameters == null) {
			call.getSender()
					.sendMessage(Prism.messenger.playerError("You must specify parameters, at least one player."));
			return;
		}
		// No actions
		if (!parameters.getActionTypes().isEmpty()) {
			call.getSender()
					.sendMessage(Prism.messenger.playerError("You may not specify any action types for this report."));
			return;
		}
		// Verify single player name for now
		final HashMap<String, MatchRule> players = parameters.getPlayerNames();
		if (players.size() != 1) {
			call.getSender().sendMessage(Prism.messenger.playerError("You must provide only a single player name."));
			return;
		}

		final BlockReportQuery reportQuery = Prism.getPrismDataSource().createBlockReportQuery();
		/*
		  Run the lookup itself in an async task so the lookup query isn't done on the
		  main thread
		 */
		plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> reportQuery.report(call.getSender()));
	}

	/**
	 * 
	 * @param call  Sender
	 */
	protected void actionTypeCountReport(final CallInfo call) {

		// Process and validate all of the arguments
		final QueryParameters parameters = PreprocessArgs.process(plugin, call.getSender(), call.getArgs(),
				PrismProcessType.LOOKUP, 3, !plugin.getConfig().getBoolean("prism.queries.never-use-defaults"));
		if (parameters == null) {
			return;
		}

		// No actions
		if (!parameters.getActionTypes().isEmpty()) {
			call.getSender()
					.sendMessage(Prism.messenger.playerError("You may not specify any action types for this report."));
			return;
		}

		// Verify single player name for now
		final HashMap<String, MatchRule> players = parameters.getPlayerNames();
		if (players.size() != 1) {
			call.getSender().sendMessage(Prism.messenger.playerError("You must provide only a single player name."));
			return;
		}

		final ActionReportQuery reportQuery = Prism.getPrismDataSource().createActionReportQuery();


		/*
		  Run the lookup itself in an async task so the lookup query isn't done on the
		  main thread
		 */
		plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> reportQuery.report(call.getSender()));
	}
}