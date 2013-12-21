package me.botsko.prism.actionlibs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Handler;

public class RecordingTask implements Runnable {
	
	/**
	 * 
	 */
	private Prism plugin;

	
	/**
	 * 
	 * @param plugin
	 */
	public RecordingTask( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 */
	public void save(){
		if(!RecordingQueue.getQueue().isEmpty()){
			insertActionsIntoDatabase();
		}
	}
	
	
	/**
	 * 
	 * @param a
	 */
	public static int insertActionIntoDatabase( Handler a){
		int id = 0;
		Connection conn = null;
		PreparedStatement s = null;
		ResultSet generatedKeys = null;
		try {
			
			// prepare to save to the db
			a.save();

			conn = Prism.dbc();
			if(conn == null){
				Prism.log("Prism database error. Connection should be there but it's not. This action wasn't logged.");
				return 0;
			}
			
			int world_id = 0;
        	if( Prism.prismWorlds.containsKey(a.getWorldName()) ){
        		world_id = Prism.prismWorlds.get(a.getWorldName());
        	}
        	
        	int action_id = 0;
        	if( Prism.prismActions.containsKey(a.getType().getName()) ){
        		action_id = Prism.prismActions.get(a.getType().getName());
        	}
        	
        	int player_id = getPlayerPrimaryKey( a.getPlayerName() );
        	
        	if( world_id == 0 || action_id == 0 || player_id == 0 ){
        		// @todo do something, error here
        	}
			
	        s = conn.prepareStatement("INSERT INTO prism_data (epoch,action_id,player_id,world_id,block_id,block_subid,old_block_id,old_block_subid,x,y,z) VALUES (?,?,?,?,?,?,?,?,?,?,?)", Statement.RETURN_GENERATED_KEYS);
	        s.setLong(1, System.currentTimeMillis() / 1000L);
	        s.setInt(2,world_id);
	        s.setInt(3,player_id);
	        s.setInt(4,world_id);
	        s.setInt(5,a.getBlockId());
	        s.setInt(6,a.getBlockSubId());
	        s.setInt(7,a.getOldBlockId());
	        s.setInt(8,a.getOldBlockSubId());
	        s.setInt(9,(int)a.getX());
	        s.setInt(10,(int)a.getY());
	        s.setInt(11,(int)a.getZ());
	        s.executeUpdate();
	        
	        generatedKeys = s.getGeneratedKeys();
	        if(generatedKeys.next()){
	        	id = generatedKeys.getInt(1);
	        }
	        
	        // Add insert query for extra data if needed
			if( a.getData() != null && !a.getData().isEmpty() ){
				s = conn.prepareStatement("INSERT INTO prism_data_extra (data_id,data) VALUES (?,?)");
				s.setInt(1, id);
				s.setString(2, a.getData());
				s.executeUpdate();
			}
	        
        } catch (SQLException e) {
//        	plugin.handleDatabaseException( e );
        } finally {
        	if(generatedKeys != null) try { generatedKeys.close(); } catch (SQLException e) {}
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
		return id;
	}
	
	
	/**
	 * 
	 * @param playerName
	 * @return
	 */
	protected static int getPlayerPrimaryKey( String playerName ){
		int player_id = 0;
    	if( Prism.prismPlayers.containsKey(playerName) ){
    		player_id = Prism.prismPlayers.get(playerName);
    	} else {
    		Prism.cachePlayerPrimaryKey(playerName);
    		player_id = Prism.prismPlayers.get(playerName);
    	}
    	return player_id;
	}
	
	
	/**
	 *
	 * @throws SQLException
	 */
	public void insertActionsIntoDatabase() {
		
	    PreparedStatement s = null;
	    Connection conn = null;
	    
	    int actionsRecorded = 0;
	    try {

	    	int perBatch = plugin.getConfig().getInt("prism.database.actions-per-insert-batch");
	    	if(perBatch < 1) perBatch = 1000;
	    	
	    	if( !RecordingQueue.getQueue().isEmpty() ){

	    		ArrayList<Handler> extraDataQueue = new ArrayList<Handler>();
		    	conn = Prism.dbc();
		    	
		    	// Handle dead connections
		        if( conn == null || conn.isClosed() ){
		        	if( RecordingManager.failedDbConnectionCount == 0 ){
		        		Prism.log("Prism database error. Connection should be there but it's not. Leaving actions to log in queue.");
		        	}
		        	RecordingManager.failedDbConnectionCount++;
					if( RecordingManager.failedDbConnectionCount > plugin.getConfig().getInt("prism.database.max-failures-before-wait") ){
//						RecordingManager.lastPauseTime = System.currentTimeMillis();
						Prism.log("Too many problems connecting. Let's wait for "+plugin.getConfig().getInt("prism.database.wait-on-failure-duration")+" seconds before we try again.");
						scheduleNextRecording();
					}
					return;
				} else {
					RecordingManager.failedDbConnectionCount = 0;
				}
		        
		        // Connection valid, proceed
		        conn.setAutoCommit(false);
		        s = conn.prepareStatement("INSERT INTO prism_data (epoch,action_id,player_id,world_id,block_id,block_subid,old_block_id,old_block_subid,x,y,z) VALUES (?,?,?,?,?,?,?,?,?,?,?)", Statement.RETURN_GENERATED_KEYS);
		        int i = 0;
		        while (!RecordingQueue.getQueue().isEmpty()){
		        	
		        	Handler a = RecordingQueue.getQueue().poll();
		        	
		        	if( a == null ){
		        		Prism.log("Action to be recorded was null.");
		        		continue;
		        	}
		        	
		        	int world_id = 0;
		        	if( Prism.prismWorlds.containsKey(a.getWorldName()) ){
		        		world_id = Prism.prismWorlds.get(a.getWorldName());
		        	}
		        	
		        	int action_id = 0;
		        	if( Prism.prismActions.containsKey(a.getType().getName()) ){
		        		action_id = Prism.prismActions.get(a.getType().getName());
		        	}
		        	
		        	int player_id = getPlayerPrimaryKey( a.getPlayerName() );
		        	
		        	if( world_id == 0 || action_id == 0 || player_id == 0 ){
		        		// @todo do something, error here
		        		Prism.log("Cache data was empty. Please report to developer: world_id:"+world_id+"/"+a.getWorldName()+" action_id:"+action_id+"/"+a.getType().getName()+" player_id:"+player_id+"/"+a.getPlayerName());
		        		Prism.log("HOWEVER, this likely means you have a broken prism database installation.");
		        		continue;
		        	}
		        	
		        	if( a == null || a.isCanceled() ) continue;
		        	
		        	actionsRecorded++;
		        	
		        	s.setLong(1, System.currentTimeMillis() / 1000L);
			        s.setInt(2,action_id);
			        s.setInt(3,player_id);
			        s.setInt(4,world_id);
			        s.setInt(5,a.getBlockId());
			        s.setInt(6,a.getBlockSubId());
			        s.setInt(7,a.getOldBlockId());
			        s.setInt(8,a.getOldBlockSubId());
			        s.setInt(9,(int)a.getX());
			        s.setInt(10,(int)a.getY());
			        s.setInt(11,(int)a.getZ());
		            s.addBatch();

		            extraDataQueue.add( a );

		            if ((i + 1) % perBatch == 0) {
		            	
		            	Prism.debug("Recorder: Batch max exceeded, running insert. Queue remaining: " + RecordingQueue.getQueue().size());
		                s.executeBatch(); // Execute every x items.
		                insertExtraData( conn, extraDataQueue, s.getGeneratedKeys() );
		                
		            }
		            i++;
		        }
		        
		        // Save the current count to the queue for short historical data
		        plugin.queueStats.addRunCount(actionsRecorded);
		        
		        s.executeBatch();
		        insertExtraData( conn, extraDataQueue, s.getGeneratedKeys() );
		        conn.commit();

	    	}
	    } catch (SQLException e){
	    	e.printStackTrace();
	    	plugin.handleDatabaseException( e );
        } finally {
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
	}
	
	
	/**
	 * 
	 * @param keys
	 * @throws SQLException 
	 */
	protected void insertExtraData( Connection conn, ArrayList<Handler> extraDataQueue, ResultSet keys ) throws SQLException{
		
		if( extraDataQueue.isEmpty() ) return;

		PreparedStatement s = null;
	    
	    int rowcount = 0;
	    if(keys.last()){
	    	rowcount = keys.getRow();
	    	keys.beforeFirst();
	    }
	    
	    if( rowcount != extraDataQueue.size() ){
	    	Prism.log("Please report to prism devs: Extra data queue did not equal keys returned. keys: " + rowcount + " extra data queue: " + extraDataQueue.size() );
	    }

	    try {
	        s = conn.prepareStatement("INSERT INTO prism_data_extra (data_id,data) VALUES (?,?)");
	        int i = 0;
			while(keys.next()){

				// @todo should not happen
				if( i >= extraDataQueue.size() ){
					Prism.log( "Skipping extra data for prism_data.id " + keys.getInt(1) + " because the queue doesn't have data for it." );
					continue;
				}

				Handler a = extraDataQueue.get(i);
				
				if( a.getData() != null && !a.getData().isEmpty() ){
					s.setInt(1, keys.getInt(1));
					s.setString(2, a.getData());
					s.addBatch();
				}
				
				i++;
				
			}
			s.executeBatch();

	    } catch (SQLException e){
	    	e.printStackTrace();
	    	plugin.handleDatabaseException( e );
        } finally {
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	// conn close handled by parent method
        }
	}


	/**
	 * 
	 */
	public void run(){
		if( RecordingManager.failedDbConnectionCount > 5 ){
			plugin.rebuildPool(); //force rebuild pool after several failures
		}
		save();
		scheduleNextRecording();
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected int getTickDelayForNextBatch(){
		
		// If we have too many rejected connections, increase the schedule
		if( RecordingManager.failedDbConnectionCount > plugin.getConfig().getInt("prism.database.max-failures-before-wait") ){
			return RecordingManager.failedDbConnectionCount * 20;
		}
		
		int recorder_tick_delay = plugin.getConfig().getInt("prism.queue-empty-tick-delay");
		if (recorder_tick_delay < 1) {
			recorder_tick_delay = 3;
		}
		return recorder_tick_delay;
	}
	
	
	/**
	 * 
	 */
	protected void scheduleNextRecording(){
//		Prism.debug("Scheduling next recording task in " + getTickDelayForNextBatch() + " ticks.");
		plugin.recordingTask = plugin.getServer().getScheduler().runTaskLaterAsynchronously(plugin,new RecordingTask(plugin), getTickDelayForNextBatch());
	}
}
