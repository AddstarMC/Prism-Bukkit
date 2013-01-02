package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;

public class EntityKillAction extends BaseAction {

	
	/**
	 * 
	 */
	protected Entity entity;


	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public EntityKillAction( String action_type, Entity entity, String player ){
		if(action_type != null){
			this.action_type = action_type;
		}
		if(entity != null){
			this.entity = entity;
			this.world_name = entity.getWorld().getName();
			this.x = entity.getLocation().getX();
			this.y = entity.getLocation().getY();
			this.z = entity.getLocation().getZ();
		}
		if(player != null){
			this.player_name = player;
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
		// Save entity data from current entity
		setDataFromEntity();
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromEntity(){
		if(data == null && entity != null){
			data = entity.getType().getName();
		}
	}
	
	
	/**
	 * 
	 */
	public EntityType getEntityTypeFromData(){
		if(entity == null){
			EntityType mob = EntityType.fromName(data);
			if(mob != null){
				return mob;
			}
		}
		return null;
	}
}