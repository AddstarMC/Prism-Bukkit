package me.botsko.prism.commands;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentSkipListMap;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.scheduler.BukkitScheduler;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.RecordingManager;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.database.mysql.ActionReportQueryBuilder;
import me.botsko.prism.database.mysql.BlockReportQueryBuilder;

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
		
		if(call.getArgs().length < 2){
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
			
			if( call.getArg(2).equals("blocks") ){
				blockSumReports( call );
			}
			
			if( call.getArg(2).equals("actions") ){
				actionTypeCountReport( call );
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
				sender.sendMessage( Prism.messenger.playerSuccess( "Pool returned valid connection!") );
			}
		} catch (SQLException e){
			sender.sendMessage( Prism.messenger.playerError( "Error: " + e.getMessage()) );
	    	e.printStackTrace();
        } finally {
        	if(conn != null) try { conn.close(); } catch (SQLException ignored) {}
        }
	}
	
	
	/**
	 * 
	 * @param sender
	 */
	protected void blockSumReports( final CallInfo call ){
		
		// Process and validate all of the arguments
		final QueryParameters parameters = PreprocessArgs.process( plugin, call.getSender(), call.getArgs(), PrismProcessType.LOOKUP, 3, !plugin.getConfig().getBoolean("prism.queries.never-use-defaults") );
		if(parameters == null){
			call.getSender().sendMessage( Prism.messenger.playerError("You must specify parameters, at least one player.") );
			return;
		}
		
		// No actions
		if( !parameters.getActionTypes().isEmpty() ){
			call.getSender().sendMessage( Prism.messenger.playerError("You may not specify any action types for this report.") );
			return;
		}

		// Verify single player name for now
		HashMap<String,MatchRule> players = parameters.getPlayerNames();
		if( players.size() != 1 ){
			call.getSender().sendMessage( Prism.messenger.playerError("You must provide only a single player name.") );
			return;
		}
		// Get single playername
		String tempName = "";
		for( String player : players.keySet() ){
			tempName = player;
			break;
		}
		final String playerName = tempName;
		
		
		BlockReportQueryBuilder reportQuery = new BlockReportQueryBuilder(plugin);
		final String sql = reportQuery.getQuery(parameters, false);
		
		final int colTextLen = 20;
		final int colIntLen = 12;
		
		/**
		 * Run the lookup itself in an async task so the lookup query isn't done on the main thread
		 */
		plugin.getServer().getScheduler().runTaskAsynchronously(plugin, new Runnable(){
			public void run(){
				
				call.getSender().sendMessage( Prism.messenger.playerSubduedHeaderMsg("Crafting block change report for " + ChatColor.DARK_AQUA + playerName + "...") );

				Connection conn = null;
				PreparedStatement s = null;
				ResultSet rs = null;
				try {

					conn = Prism.dbc();
		    		s = conn.prepareStatement(sql);
		    		s.executeQuery();
		    		rs = s.getResultSet();
		    		
		    		call.getSender().sendMessage( Prism.messenger.playerHeaderMsg("Total block changes for " + ChatColor.DARK_AQUA + playerName) );
		    		call.getSender().sendMessage( Prism.messenger.playerMsg( ChatColor.GRAY + TypeUtils.padStringRight( "Block", colTextLen ) + TypeUtils.padStringRight( "Placed", colIntLen ) + TypeUtils.padStringRight( "Broken", colIntLen ) ) );

		    		while(rs.next()){
		    			
		    			String alias = Prism.getItems().getAlias(rs.getInt(1), 0);
	
		    			int placed = rs.getInt(2);
		    			int broken = rs.getInt(3);
		    			
		    			String colAlias = TypeUtils.padStringRight( alias, colTextLen );
		    			String colPlaced = TypeUtils.padStringRight( ""+placed, colIntLen );
		    			String colBroken = TypeUtils.padStringRight( ""+broken, colIntLen );
		    			
		    			call.getSender().sendMessage( Prism.messenger.playerMsg( ChatColor.DARK_AQUA + colAlias + ChatColor.GREEN + colPlaced + " " + ChatColor.RED + colBroken ) );
		    			
		    		}
		        } catch (SQLException e) {
		        	e.printStackTrace();
		        } finally {
		        	if(rs != null) try { rs.close(); } catch (SQLException ignored) {}
		        	if(s != null) try { s.close(); } catch (SQLException ignored) {}
		        	if(conn != null) try { conn.close(); } catch (SQLException ignored) {}
		        }
			}
		});
	}
	
	
	/**
	 * 
	 * @param sender
	 */
	protected void actionTypeCountReport( final CallInfo call ){

		// Process and validate all of the arguments
		final QueryParameters parameters = PreprocessArgs.process( plugin, call.getSender(), call.getArgs(), PrismProcessType.LOOKUP, 3, !plugin.getConfig().getBoolean("prism.queries.never-use-defaults") );
		if(parameters == null){
			return;
		}
		
		// No actions
		if( !parameters.getActionTypes().isEmpty() ){
			call.getSender().sendMessage( Prism.messenger.playerError("You may not specify any action types for this report.") );
			return;
		}

		// Verify single player name for now
		HashMap<String,MatchRule> players = parameters.getPlayerNames();
		if( players.size() != 1 ){
			call.getSender().sendMessage( Prism.messenger.playerError("You must provide only a single player name.") );
			return;
		}
		// Get single playername
		String tempName = "";
		for( String player : players.keySet() ){
			tempName = player;
			break;
		}
		final String playerName = tempName;
		
		
		ActionReportQueryBuilder reportQuery = new ActionReportQueryBuilder(plugin);
		final String sql = reportQuery.getQuery(parameters, false);
		
		final int colTextLen = 16;
		final int colIntLen = 12;
		
		/**
		 * Run the lookup itself in an async task so the lookup query isn't done on the main thread
		 */
		plugin.getServer().getScheduler().runTaskAsynchronously(plugin, new Runnable(){
			public void run(){
				
				call.getSender().sendMessage( Prism.messenger.playerSubduedHeaderMsg("Crafting action type report for " + ChatColor.DARK_AQUA + playerName + "...") );

				Connection conn = null;
				PreparedStatement s = null;
				ResultSet rs = null;
				try {

					conn = Prism.dbc();
		    		s = conn.prepareStatement(sql);
		    		s.executeQuery();
		    		rs = s.getResultSet();
		    		
		    		call.getSender().sendMessage( Prism.messenger.playerMsg( ChatColor.GRAY + TypeUtils.padStringRight( "Action", colTextLen ) + TypeUtils.padStringRight( "Count", colIntLen ) ) );

		    		while(rs.next()){
		    			
		    			String action = rs.getString(2);
		    			int count = rs.getInt(1);
		    			
		    			String colAlias = TypeUtils.padStringRight( action, colTextLen );
		    			String colPlaced = TypeUtils.padStringRight( ""+count, colIntLen );
		    			
		    			call.getSender().sendMessage( Prism.messenger.playerMsg( ChatColor.DARK_AQUA + colAlias + ChatColor.GREEN + colPlaced ) );
		    			
		    		}
		        } catch (SQLException e) {
		        	e.printStackTrace();
		        } finally {
		        	if(rs != null) try { rs.close(); } catch (SQLException ignored) {}
		        	if(s != null) try { s.close(); } catch (SQLException ignored) {}
		        	if(conn != null) try { conn.close(); } catch (SQLException ignored) {}
		        }
			}
		});
	}
}