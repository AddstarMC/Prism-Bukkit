package me.botsko.prism.actionlibs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.concurrent.LinkedBlockingQueue;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Action;

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
	public void addToQueue( Action a ){
		
		if(a == null) return;
		
		queue.add(a);
		
		if(a.getData() != null && a.getData().length() > 255){
			plugin.log("Error: Data exceeds allowed length and will not be logged. Please inform Prism developers: " + a.getData());
		}
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
