package me.botsko.prism.recorders;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.concurrent.LinkedBlockingQueue;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Action;

public class ActionRecorder {
	
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
	 * @param a
	 */
	public void addToQueue( Action a ){
		
		queue.add(a);
		
		plugin.debug( a.getAction_type() + " in " + a.getWorld_name() + " at " + a.getX() + " " + a.getY() + " " + a.getZ() + " by " + a.getPlayer_name() );
		
		// @todo for now, we're just calling save immediately. but we could
		// make this work on a timer, pool, etc
		save();
		
	}
	
	
	/**
	 * 
	 */
	public void save(){
		if(!queue.isEmpty()){
			while (!queue.isEmpty()) {
				Action a = queue.poll();
				insertActionIntoDatabase( a );
				queue.remove(a); //@todo unecessary?
			}
		} else {
			plugin.debug("Action queue empty when save() called.");
		}
	}
	
	
	/**
	 * 
	 * @param a
	 */
	protected void insertActionIntoDatabase( Action a){
		try {
			plugin.dbc();
	        PreparedStatement s = plugin.conn.prepareStatement("INSERT INTO prism_actions (action_time,action_type,player,world,x,y,z,data) VALUES (?,?,?,?,?,?,?,?)");
	        s.setString(1,a.getAction_time());
	        s.setString(2,a.getAction_type());
	        s.setString(3,a.getPlayer_name());
	        s.setString(4,a.getWorld_name());
	        s.setInt(5,(int)a.getX());
	        s.setInt(6,(int)a.getY());
	        s.setInt(7,(int)a.getZ());
	        s.setString(8,a.getData());
	        s.executeUpdate();
    		s.close();
            plugin.conn.close();
        } catch (SQLException e) {
            e.printStackTrace();
        }
	}
}
