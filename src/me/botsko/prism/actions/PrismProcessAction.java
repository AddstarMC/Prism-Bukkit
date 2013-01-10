package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import me.botsko.prism.appliers.PrismProcessType;

import org.bukkit.entity.Player;


public class PrismProcessAction extends GenericAction {
	
	/**
	 * 
	 */
	private PrismProcessActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public PrismProcessAction( ActionType action_type, PrismProcessType processType, Player player, String parameters ){
		
		actionData = new PrismProcessActionData();
		
		// Store information for the action
		if(action_type != null){
			this.type = action_type;
		}
		if(processType != null){
			actionData.params = parameters;
			actionData.processType = processType.name().toLowerCase();
		}
		if(player != null){
			this.player_name = player.getName();
			this.world_name = player.getWorld().getName();
			this.x = player.getLocation().getX();
			this.y = player.getLocation().getY();
			this.z = player.getLocation().getZ();
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
		
		// Set data from current block
		setDataFromObject();
		setObjectFromData();
		
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setObjectFromData();
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromObject(){
		data = gson.toJson(actionData);
	}
	
	
	/**
	 * 
	 */
	protected void setObjectFromData(){
		if(data != null){
			actionData = gson.fromJson(data, PrismProcessActionData.class);
		}
	}
	
	
	/**
	 * 
	 */
	public String getNiceName(){
		return actionData.processType + " ("+actionData.params+")";
	}
}