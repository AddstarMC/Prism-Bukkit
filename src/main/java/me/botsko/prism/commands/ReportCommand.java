package me.botsko.prism.commands;

import java.sql.Connection;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentSkipListMap;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.scheduler.BukkitScheduler;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.RecordingManager;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class ReportCommand implements SubHandler {
	
	/**
	 * 
	 */
	private final Prism plugin;
	
	
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
			call.getSender().sendMessage( Prism.messenger.playerError( "Please specify a report. Use /prism ? for help." ) );
			return;
		}
		
		// /prism report queue
		if(call.getArg(1).equals("queue")){
			queueReport( call.getSender() );
		}
		
		// /prism report db
		if(call.getArg(1).equals("db")){
			databaseReport( call.getSender() );
		}
		
		// /prism report queue
		if(call.getArg(1).equals("sum")){
			
			if( call.getArgs().length < 3 ){
				call.getSender().sendMessage( Prism.messenger.playerError( "Please specify a 'sum' report. Use /prism ? for help." ) );
				return;
			}
			
			if( call.getArgs().length < 4 ){
				call.getSender().sendMessage( Prism.messenger.playerError( "Please provide a player name. Use /prism ? for help." ) );
				return;
			}
		}
	}
	
	
	/**
	 * 
	 * @param sender
	 */
	protected void queueReport( CommandSender sender ){
		
		sender.sendMessage( Prism.messenger.playerHeaderMsg( "Current Stats") );
		
		sender.sendMessage( Prism.messenger.playerMsg( "Actions in queue: " + ChatColor.WHITE + RecordingQueue.getQueueSize() ) );
		
		ConcurrentSkipListMap<Long,Integer> runs = plugin.queueStats.getRecentRunCounts();
		if(runs.size() > 0){
			sender.sendMessage( Prism.messenger.playerHeaderMsg( "Recent queue save stats:" ) );
			for (Entry<Long, Integer> entry : runs.entrySet()){
				String time = new SimpleDateFormat("HH:mm:ss").format( entry.getKey());
			    sender.sendMessage( Prism.messenger.playerMsg( ChatColor.GRAY + time + " " + ChatColor.WHITE + entry.getValue() ) );
			}
		}
	}
	
	
	/**
	 * 
	 * @param sender
	 */
	protected void databaseReport( CommandSender sender ){
		
		sender.sendMessage( Prism.messenger.playerHeaderMsg( "Database Connection State") );
		
		sender.sendMessage( Prism.messenger.playerMsg( "Active Failure Count: " + ChatColor.WHITE + RecordingManager.failedDbConnectionCount ) );
		sender.sendMessage( Prism.messenger.playerMsg( "Actions in queue: " + ChatColor.WHITE + RecordingQueue.getQueueSize() ) );
		sender.sendMessage( Prism.messenger.playerMsg( "Pool active: " + ChatColor.WHITE + Prism.getPool().getActive() ) );
		sender.sendMessage( Prism.messenger.playerMsg( "Pool idle: " + ChatColor.WHITE + Prism.getPool().getIdle() ) );
		sender.sendMessage( Prism.messenger.playerMsg( "Pool active count: " + ChatColor.WHITE + Prism.getPool().getNumActive() ) );
		sender.sendMessage( Prism.messenger.playerMsg( "Pool idle count: " + ChatColor.WHITE + Prism.getPool().getNumIdle() ) );
		
		boolean recorderActive = false;
		if( plugin.recordingTask != null ){
			int taskId = plugin.recordingTask.getTaskId();
			BukkitScheduler scheduler = Bukkit.getScheduler();
			if( scheduler.isCurrentlyRunning(taskId) || scheduler.isQueued(taskId) ){
				recorderActive = true;
			}
		}
		
		if( recorderActive ){
			sender.sendMessage( Prism.messenger.playerSuccess( "Recorder is currently queued or running!") );
		} else {
			sender.sendMessage( Prism.messenger.playerError( "Recorder stopped running! DB conn problems? Try /pr recorder start") );
		}
		
		sender.sendMessage( Prism.messenger.playerSubduedHeaderMsg( "Attempting to check connection readiness...") );
		
		Connection conn = null;
		try {
			
			conn = Prism.dbc();
			if( conn == null ){
				sender.sendMessage( Prism.messenger.playerError( "Pool returned NULL instead of a valid connection.") );
			}
			else if( conn.isClosed() ){
				sender.sendMessage( Prism.messenger.playerError( "Pool returned an already closed connection.") );
			}
			else if( conn.isValid(5) ){
				sender.sendMessage( Prism.messenger.playerMsg( "Pool returned valid connection!") );
			}
		} catch (SQLException e){
			sender.sendMessage( Prism.messenger.playerError( "Error: " + e.getMessage()) );
	    	e.printStackTrace();
        } finally {
        	if(conn != null) try { conn.close(); } catch (SQLException ignored) {}
        }
	}
}