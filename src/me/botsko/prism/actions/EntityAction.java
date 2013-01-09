package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.DyeColor;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Sheep;

public class EntityAction extends GenericAction {

	
	/**
	 * 
	 */
	protected EntityActionData actionData;


	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public EntityAction( ActionType action_type, Entity entity, String player ){
		
		// Build an object for the specific details of this action
		actionData = new EntityActionData();
				
		if(action_type != null){
			this.type = action_type;
		}
		if(entity != null){
			this.actionData.entity_name = entity.getType().getName().toLowerCase();
			this.world_name = entity.getWorld().getName();
			this.x = entity.getLocation().getX();
			this.y = entity.getLocation().getY();
			this.z = entity.getLocation().getZ();
			
			// Get sheep color
			if( entity.getType().equals(EntityType.SHEEP)){
				Sheep sheep = ((Sheep) entity);
				this.actionData.color = sheep.getColor().name().toLowerCase();
			}
		}
		if(player != null){
			this.player_name = player;
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
		
		// Save entity data from current entity
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
			actionData = gson.fromJson(data, EntityActionData.class);
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public EntityType getEntityType(){
		return EntityType.valueOf(actionData.entity_name.toUpperCase());
	}
	
	
	/**
	 * 
	 * @return
	 */
	public DyeColor getColor(){
		return DyeColor.valueOf(actionData.color.toUpperCase());
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		if(actionData.color != null && !actionData.color.isEmpty()){
			name += actionData.color + " ";
		}
		name += actionData.entity_name;
		return name;
	}
}