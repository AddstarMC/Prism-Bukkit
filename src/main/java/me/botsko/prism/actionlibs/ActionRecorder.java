package me.botsko.prism.actionlibs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;

import org.bukkit.GameMode;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Action;
import me.botsko.prism.utils.TypeUtils;

public class ActionRecorder implements Runnable {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private static final LinkedBlockingQueue<Action> queue = new LinkedBlockingQueue<Action>();
	
	/**
	 * 
	 */
	private final List<String> ignore_players;
	
	/**
	 * 
	 */
	private final List<String> ignore_worlds;
	
	/**
	 * 
	 */
	private final boolean ignore_creative;

	
	/**
	 * 
	 * @param plugin
	 */
	@SuppressWarnings("unchecked")
	public ActionRecorder( Prism plugin ){
		this.plugin = plugin;
		ignore_players = (List<String>) plugin.getConfig().getList( "prism.ignore.players" );
		ignore_worlds = (List<String>) plugin.getConfig().getList( "prism.ignore.worlds" );
		ignore_creative = plugin.getConfig().getBoolean( "prism.ignore.players-in-creative" );
	}
	
	
	/**
	 * 
	 */
	public int getQueueSize(){
		return queue.size();
	}
	
	
	/**
	 * 
	 * @param a
	 */
	public void addToQueue( Action a ){
		
		if(a == null) return;
		
		// Verify we're expected to track this action, world, player
		if(!shouldTrack(a)){
			return;
		}
		
		queue.add(a);
		
		if(a.getData() != null && a.getData().length() > 255){
			plugin.log("Error: Data exceeds allowed length and will not be logged. Please inform Prism developers: " + a.getData());
		}
	}
	
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	protected boolean shouldTrack( Action a ){
		
		// Always track Prism actions - it's mainly internal
		// use anyway.
		if(a.getType().getName().contains("prism")){
			return true;
		}
		
		// Should we ignore this player?
		if(ignore_players != null && ignore_players.contains( a.getPlayerName() )){
			return false;
		}
		
		// Should we ignore this world?
		if(ignore_worlds != null && ignore_worlds.contains( a.getWorldName() )){
			return false;
		}
		
		// Should we ignore this action type?
		String action_type = a.getType().getName();
		if( (TypeUtils.subStrOccurences(action_type, "-") == 1 && !plugin.getConfig().getBoolean( "prism.tracking." + action_type )) ){
			return false;
		}
		
		// Should we ignore this player for being in creative?
		// @todo maybe we should pass the full player to actions.
		if( ignore_creative ){
			String name = a.getPlayerName();
			if(name == null) return true;
			Player pl = plugin.getServer().getPlayer( name );
			if(pl != null && pl.getGameMode().equals(GameMode.CREATIVE)){
				return false;
			}
		}
		return true;
	}
	
	
	/**
	 * 
	 */
	public void save(){
//		plugin.debug("Recorder: Checking queue for pending inserts. Queue size: " + queue.size());
		if(!queue.isEmpty()){
			insertActionsIntoDatabase();
		}
	}
	
	
	/**
	 * 
	 * @param a
	 */
	public int insertActionIntoDatabase( Action a){
		int id = 0;
		try {

			Connection conn = Prism.dbc();
			if(conn == null){
				plugin.log("Prism database error. Connection should be there but it's not. This action wasn't logged.");
				return 0;
			}
	        PreparedStatement s = conn.prepareStatement("INSERT INTO prism_actions (action_type,player,world,block_id,block_subid,x,y,z,data) VALUES (?,?,?,?,?,?,?,?,?)", Statement.RETURN_GENERATED_KEYS);
	        s.setString(1,a.getType().getName());
	        s.setString(2,a.getPlayerName());
	        s.setString(3,a.getWorldName());
	        s.setInt(4,a.getBlockId());
	        s.setInt(5,a.getBlockSubId());
	        s.setInt(6,(int)a.getX());
	        s.setInt(7,(int)a.getY());
	        s.setInt(8,(int)a.getZ());
	        s.setString(9,a.getData());
	        s.executeUpdate();
	        
	        ResultSet generatedKeys = s.getGeneratedKeys();
	        if(generatedKeys.next()){
	        	id = generatedKeys.getInt(1);
	        }
	        
	        generatedKeys.close();
    		s.close();
    		conn.close();
        } catch (SQLException e) {
        	plugin.logDbError( e );
        }
		return id;
	}
	
	
	/**
	 * 
	 * @param entities
	 * @throws SQLException
	 */
	public void insertActionsIntoDatabase() {
	    PreparedStatement s = null;
	    int actionsRecorded = 0;
	    try {
	    	
	    	plugin.debug("Pool: MaxActive: " + Prism.getPool().getMaxActive() + " MaxIdle: " + Prism.getPool().getMaxIdle() + " Idle: " + Prism.getPool().getNumIdle() + " Active: " + Prism.getPool().getNumActive());
	    	
	    	int perBatch = plugin.getConfig().getInt("prism.database.actions-per-insert-batch");
	    	if(perBatch < 0 || perBatch > 5000){
	    		perBatch = 1000;
	    	}
	    	
	    	if( !queue.isEmpty() ){

		    	Connection conn = Prism.dbc();
		        if(conn == null || conn.isClosed()){
					plugin.log("Prism database error. Connection should be there but it's not. Leaving actions to log in queue.");
					return;
				}
		        conn.setAutoCommit(false);
		        s = conn.prepareStatement("INSERT INTO prism_actions (action_type,player,world,block_id,block_subid,x,y,z,data) VALUES (?,?,?,?,?,?,?,?,?)");
		        int i = 0;
		        while (!queue.isEmpty()){
		        	actionsRecorded++;
		        	Action a = queue.poll();
		        	if(a == null) continue;
			        s.setString(1,a.getType().getName());
			        s.setString(2,a.getPlayerName());
			        s.setString(3,a.getWorldName());
			        s.setInt(4,a.getBlockId());
			        s.setInt(5,a.getBlockSubId());
			        s.setInt(6,(int)a.getX());
			        s.setInt(7,(int)a.getY());
			        s.setInt(8,(int)a.getZ());
			        s.setString(9,a.getData());
		            s.addBatch();
		            if ((i + 1) % perBatch == 0) {
		            	plugin.debug("Recorder: Batch max exceeded, running insert. Queue remaining: " + queue.size());
		                s.executeBatch(); // Execute every x items.
		            }
		            i++;
		        }
		        
//		        plugin.debug("Recorder: Queue emptied into single batch. Size: " + i);
		        
		        // Save the current count to the queue for short historical data
		        plugin.queueStats.addRunCount(actionsRecorded);
		        
		        s.executeBatch();
		        conn.commit();
		        s.close();
		        conn.close();
	    	}
	    } catch (SQLException e) {
	    	plugin.logDbError( e );
        }
	}


	/**
	 * 
	 */
	public void run() {
		save();
	}
}
