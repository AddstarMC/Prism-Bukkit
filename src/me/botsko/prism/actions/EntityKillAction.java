package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;

public class EntityKillAction implements Action {
	
	/**
	 * 
	 */
	protected String action_time;
	
	/**
	 * 
	 */
	protected String action_type;
	
	/**
	 * 
	 */
	protected String world_name;
	
	/**
	 * 
	 */
	protected String player_name;
	
	/**
	 * 
	 */
	protected double x;
	
	/**
	 * 
	 */
	protected double y;
	
	/**
	 * 
	 */
	protected double z;
	
	/**
	 * 
	 */
	protected Entity entity;
	
	/**
	 * 
	 */
	protected String data;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public EntityKillAction( String action_type, Entity entity, String player ){
		this(null, action_type, entity.getWorld().getName(), player, entity.getLocation().getX(), entity.getLocation().getY(), entity.getLocation().getZ(), entity, null);
	}
	
	
	/**
	 * Called when returned results from the database
	 * 
	 * @param action_time
	 * @param action_type
	 * @param world_name
	 * @param player_name
	 * @param x
	 * @param y
	 * @param z
	 * @param data
	 */
	public EntityKillAction( String action_time, String action_type, String world_name, String player_name, double x, double y, double z, String data ){
		this(action_time, action_type, world_name, player_name, x, y, z, null, data);
	}


	/**
	 * 
	 * @param action_time
	 * @param action_type
	 * @param world_name
	 * @param player_name
	 * @param x
	 * @param y
	 * @param z
	 * @param block
	 * @param data
	 */
	public EntityKillAction( String action_time, String action_type, String world_name, String player_name, double x, double y, double z, Entity entity, String data ){
		
		// @todo has to be a better place for this
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
		
		this.action_time = action_time;
		this.action_type = action_type;
		this.world_name = world_name;
		this.player_name = player_name;
		this.data = data;
		this.entity = entity;
		this.x = x;
		this.y = y;
		this.z = z;
		
		// We either received a block or a data string. We need both, so make whichever we need.
		setDataFromEntity();
		
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromEntity(){
		if(data == null){
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
	
	
	/**
	 * 
	 * @return
	 */
	public String getAction_time(){
		return action_time;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getAction_type(){
		return action_type;
	}
	
	
	/**
	 * @return the world_name
	 */
	public String getWorld_name() {
		return world_name;
	}

	
	/**
	 * @return the player_name
	 */
	public String getPlayer_name() {
		return player_name;
	}

	
	/**
	 * @return the x
	 */
	public double getX() {
		return x;
	}

	
	/**
	 * @return the y
	 */
	public double getY() {
		return y;
	}

	
	/**
	 * @return the z
	 */
	public double getZ() {
		return z;
	}
	
	
	/**
	 * 
	 */
	public String getData(){
		return data;
	}
}