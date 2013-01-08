package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.block.Block;

public class UseAction extends GenericAction {
	
	
	/**
	 * 
	 * @param action_type
	 * @param block_filters
	 * @param player
	 */
	public UseAction( ActionType action_type, String item_used, Block block, String player ){
		// Store information for the action
		if(action_type != null){
			this.type = action_type;
		}
		if(block != null){
			this.data = item_used;
			this.world_name = block.getWorld().getName();
			this.x = block.getLocation().getX();
			this.y = block.getLocation().getY();
			this.z = block.getLocation().getZ();
		}
		if(player != null){
			this.player_name = player;
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