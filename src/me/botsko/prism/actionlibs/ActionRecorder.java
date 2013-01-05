package me.botsko.prism.actionlibs;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;

import org.bukkit.GameMode;
import org.bukkit.entity.Player;

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
		
		// Verify we're expected to track this action, world, player
		if(!shouldTrack(a)){
			return;
		}
		
		queue.add(a);
		
		plugin.debug( a.getType().getActionType() + " in " + a.getWorld_name() + " at " + a.getX() + " " + a.getY() + " " + a.getZ() + " by " + a.getPlayer_name() );
		
		// @todo for now, we're just calling save immediately. but we could
		// make this work on a timer, pool, etc
		save();
		
	}
	
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	protected boolean shouldTrack( Action a ){
		
		// Should we ignore this player?
		@SuppressWarnings("unchecked")
		List<String> ignore_players = (List<String>) plugin.getConfig().getList( "prism.ignore.players" );
		if(ignore_players != null && ignore_players.contains( a.getPlayer_name() )){
			return false;
		}
		
		// Should we ignore this world?
		@SuppressWarnings("unchecked")
		List<String> ignore_worlds = (List<String>) plugin.getConfig().getList( "prism.ignore.worlds" );
		if(ignore_worlds != null && ignore_worlds.contains( a.getWorld_name() )){
			return false;
		}
		
		// Should we ignore this action type?
		String action_type = a.getType().getActionType();
		if(!plugin.getConfig().getBoolean( "prism.tracking." + action_type )){
			return false;
		}
		
		// Should we ignore this player for being in creative?
		// @todo maybe we should pass the full player to actions.
		if( plugin.getConfig().getBoolean( "prism.ignore.players-in-creative" ) ){
			String name = a.getPlayer_name();
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
	        s.setString(2,a.getType().getActionType());
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
