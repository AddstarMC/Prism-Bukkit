package me.botsko.prism.actions;

import org.bukkit.Location;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause;

public class EntityTravelAction extends GenericAction {
	
	
	public class EntityTravelActionData {
		int to_x;
		int to_y;
		int to_z;
		String cause;
	}

	
	/**
	 * 
	 */
	protected Entity entity;
	
	/**
	 * 
	 */
	protected EntityTravelActionData actionData;


	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public EntityTravelAction( ActionType action_type, Entity entity, Location from, Location to, TeleportCause cause ){
		
		super(action_type, null);
		
		actionData = new EntityTravelActionData();

		if(entity != null){
			this.entity = entity;
			if(entity instanceof Player){
				this.player_name = ((Player)entity).getName();
			} else {
				this.player_name = entity.getType().name().toLowerCase();
			}
		}
		if(from != null){
			this.world_name = from.getWorld().getName();
			this.x = from.getX();
			this.y = from.getY();
			this.z = from.getZ();
		}
		if(to != null){
			actionData.to_x = to.getBlockX();
			actionData.to_y = to.getBlockY();
			actionData.to_z = to.getBlockZ();
		}
		if(cause != null){
			actionData.cause = cause.name().toLowerCase();
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
		System.out.print("SETTING setDataFromObject " + data);
	}
	
	
	/**
	 * 
	 */
	protected void setObjectFromData(){
		if(data != null){
			actionData = gson.fromJson(data, EntityTravelActionData.class);
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public EntityTravelActionData getActionData(){
		return actionData;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		if(actionData != null){
			return "using " + actionData.cause.replace("_", " ") + " to " + actionData.to_x + " " + actionData.to_y + " " + actionData.to_z;
		}
		return "teleported somewhere";
	}
}