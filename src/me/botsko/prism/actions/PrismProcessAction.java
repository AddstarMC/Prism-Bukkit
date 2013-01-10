package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.entity.Player;


public class PrismProcessAction extends GenericAction {
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public PrismProcessAction( ActionType action_type, Player player, String parameters ){
		
		// Store information for the action
		if(action_type != null){
			this.type = action_type;
		}
		if(player != null){
			this.player_name = player.getName();
			this.world_name = player.getWorld().getName();
			this.x = player.getLocation().getX();
			this.y = player.getLocation().getY();
			this.z = player.getLocation().getZ();
		}
		if(parameters != null){
			this.data = parameters;
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
	}
}