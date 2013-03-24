package me.botsko.prism.actionlibs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.concurrent.LinkedBlockingQueue;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Handler;

public class ActionRecorder implements Runnable {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private static final LinkedBlockingQueue<Handler> queue = new LinkedBlockingQueue<Handler>();
	
	/**
	 * If the recorder skips running we need to count because
	 * if this happens x times in a row, the recorder
	 * will delay itself so we don't kill the server
	 */
	private int failedDbConnectionCount = 0;
	
	/**
	 * Track the timestamp at which we last paused
	 */
	private long lastPauseTime = 0;

	
	/**
	 * 
	 * @param plugin
	 */
	public ActionRecorder( Prism plugin ){
		this.plugin = plugin;
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
	public void addToQueue( Handler a ){
		
		if(a == null) return;
		
		// prepare to save to the db
		a.save();
		
		if(a.getData() != null && a.getData().length() > 255){
			plugin.log("Error: Data exceeds allowed length and will not be logged. Please inform Prism developers: " + a.getData());
			return;
		}
		
		queue.add(a);

	}
	
	
	/**
	 * 
	 */
	public void save(){
		if(!queue.isEmpty()){
			insertActionsIntoDatabase();
		}
	}
	
	
	/**
	 * 
	 * @param a
	 */
	public int insertActionIntoDatabase( Handler a){
		int id = 0;
		try {
			
			// prepare to save to the db
			a.save();

			Connection conn = Prism.dbc();
			if(conn == null){
				plugin.log("Prism database error. Connection should be there but it's not. This action wasn't logged.");
				return 0;
			}
	        PreparedStatement s = conn.prepareStatement("INSERT INTO prism_actions (action_type,player,world,block_id,block_subid,old_block_id,old_block_subid,x,y,z,data) VALUES (?,?,?,?,?,?,?,?,?,?,?)", Statement.RETURN_GENERATED_KEYS);
	        s.setString(1,a.getType().getName());
	        s.setString(2,a.getPlayerName());
	        s.setString(3,a.getWorldName());
	        s.setInt(4,a.getBlockId());
	        s.setInt(5,a.getBlockSubId());
	        s.setInt(6,a.getOldBlockId());
	        s.setInt(7,a.getOldBlockSubId());
	        s.setInt(8,(int)a.getX());
	        s.setInt(9,(int)a.getY());
	        s.setInt(10,(int)a.getZ());
	        s.setString(11,a.getData());
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
	    	
//	    	plugin.debug("Pool: MaxActive: " + Prism.getPool().getMaxActive() + " MaxIdle: " + Prism.getPool().getMaxIdle() + " Idle: " + Prism.getPool().getNumIdle() + " Active: " + Prism.getPool().getNumActive());
	    	
	    	int perBatch = plugin.getConfig().getInt("prism.database.actions-per-insert-batch");
	    	if(perBatch < 0 || perBatch > 5000){
	    		perBatch = 1000;
	    	}
	    	
	    	if( !queue.isEmpty() ){

		    	Connection conn = Prism.dbc();
		        if(conn == null || conn.isClosed()){
					plugin.log("Prism database error. Connection should be there but it's not. Leaving actions to log in queue.");
					failedDbConnectionCount++;
					if( failedDbConnectionCount > plugin.getConfig().getInt("prism.database.max-failures-before-wait") ){
						lastPauseTime = System.currentTimeMillis();
						plugin.log("Too many problems connecting. Let's wait for "+plugin.getConfig().getInt("prism.database.wait-on-failure-duration")+" seconds before we try again.");
					}
					return;
				}
		        conn.setAutoCommit(false);
		        s = conn.prepareStatement("INSERT INTO prism_actions (action_type,player,world,block_id,block_subid,old_block_id,old_block_subid,x,y,z,data) VALUES (?,?,?,?,?,?,?,?,?,?,?)");
		        int i = 0;
		        while (!queue.isEmpty()){
		        	actionsRecorded++;
		        	Handler a = queue.poll();
		        	if( a == null || a.isCanceled() ) continue;
			        s.setString(1,a.getType().getName());
			        s.setString(2,a.getPlayerName());
			        s.setString(3,a.getWorldName());
			        s.setInt(4,a.getBlockId());
			        s.setInt(5,a.getBlockSubId());
			        s.setInt(6,a.getOldBlockId());
			        s.setInt(7,a.getOldBlockSubId());
			        s.setInt(8,(int)a.getX());
			        s.setInt(9,(int)a.getY());
			        s.setInt(10,(int)a.getZ());
			        s.setString(11,a.getData());
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
	public void run(){
		long currentTime = System.currentTimeMillis();
		long diff = currentTime - lastPauseTime;
		if( lastPauseTime > 0 ){
			// see if we need to wait
			if( diff < (plugin.getConfig().getInt("prism.database.wait-on-failure-duration")*1000) ){
				return;
			} else {
				plugin.rebuildPool();
				// Otherwise, reset the pause
				lastPauseTime = 0;
				failedDbConnectionCount = 0;
			}
		}
		save();
	}
}
