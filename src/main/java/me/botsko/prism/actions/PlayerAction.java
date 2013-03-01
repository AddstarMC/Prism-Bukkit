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
	public PlayerAction( String action_type, Player player, String additionalInfo ){
	
		super(action_type, null);

		this.data = "";

		if(additionalInfo != null && !additionalInfo.isEmpty()){
			this.data = additionalInfo;
		}
		
		if(player != null){
			this.player = player;
			this.world_name = player.getWorld().getName();
			this.x = player.getLocation().getBlockX();
			this.y = player.getLocation().getBlockY();
			this.z = player.getLocation().getBlockZ();
			this.player_name = player.getName();
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		if(!this.data.isEmpty()){
			if(this.type.getName().equals("player-join")){
				return "from " + this.data;
			} 
			else if(this.type.getName().equals("xp-pickup")){
				return this.data + " xp";
			} 
			else if(this.type.getName().equals("bucket-fill")){
				return "a " + this.data + " bucket";
			} else {
				return this.data;
			}
		}
		return "";
	}
}