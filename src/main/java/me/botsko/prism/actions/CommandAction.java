package me.botsko.prism.actions;

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
	public CommandAction( String action_type, String command, Location loc, String player_name ){
		
		super(action_type, player_name);
		
		if(command != null){
			this.data = command;
			this.world_name = loc.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
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