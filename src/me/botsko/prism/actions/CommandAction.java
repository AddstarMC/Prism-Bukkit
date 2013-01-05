package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.Location;

public class CommandAction extends GenericAction {
	
	/**
	 * 
	 */
	protected String command;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public CommandAction( ActionType action_type, String command, Location loc, String player_name ){
		if(action_type != null){
			this.type = action_type;
		}
		if(command != null){
			this.data = command;
			this.world_name = loc.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
		}
		if(player_name != null){
			this.player_name = player_name;
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		return data;
	}
}