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
	public PlayerAction( ActionType action_type, Player player, String additionalInfo ){
	
		super(action_type, null);

		this.data = "";
		if(player != null){
			this.player_name = player.getName();
		}
		if(additionalInfo != null && !additionalInfo.isEmpty()){
			this.data = additionalInfo;
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
		if(!this.data.isEmpty()){
			if(this.type.equals(ActionType.XP_PICKUP)){
				return this.data + " xp";
			} else {
				// is a join event
				return "from " + this.data;
			}
		}
		return "";
	}
}