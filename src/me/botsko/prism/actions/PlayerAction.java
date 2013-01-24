package me.botsko.prism.actions;

import org.bukkit.entity.Player;

public class PlayerAction extends GenericAction {

	
	/**
	 * 
	 */
	protected Player player;


	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public PlayerAction( ActionType action_type, Player player ){
	
		super(action_type, null);

		if(player != null){
			this.player_name = player.getName();
			this.data = "";
		}
		
		if(player != null){
			this.player = player;
			this.world_name = player.getWorld().getName();
			this.x = player.getLocation().getX();
			this.y = player.getLocation().getY();
			this.z = player.getLocation().getZ();
			this.player_name = player.getName();
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		return "";
	}
}