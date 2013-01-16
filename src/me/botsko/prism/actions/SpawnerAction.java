package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.block.Block;
import org.bukkit.block.CreatureSpawner;
import org.bukkit.entity.EntityType;

public class SpawnerAction extends GenericAction {
	
	public class SpawnerActionData {
		public String entity_type;
		public int delay;
	}
	
	/**
	 * 
	 */
	protected SpawnerActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public SpawnerAction( ActionType action_type, Block block, String player ){
		
		// Build an object for the specific details of this action
		actionData = new SpawnerActionData();
		
		// Store information for the action
		if(action_type != null){
			this.type = action_type;
		}
		if(block != null){
			CreatureSpawner s = (CreatureSpawner)block.getState();
			actionData.entity_type = s.getSpawnedType().name().toLowerCase();
			actionData.delay = s.getDelay();
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
			actionData = gson.fromJson(data, SpawnerActionData.class);
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public EntityType getEntityType(){
		return EntityType.valueOf(actionData.entity_type.toUpperCase());
	}
	
	
	/**
	 * 
	 * @return
	 */
	public int getDelay(){
		return actionData.delay;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		return actionData.entity_type + " spawner";
	}
}