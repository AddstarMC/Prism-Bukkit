package me.botsko.prism.actionlibs;

import org.bukkit.Location;
import org.bukkit.util.Vector;

public class QueryParameters {
	
	
	protected String lookup_type = "lookup";
	
	protected Location loc;
	protected Vector player_location;
	
	protected int radius;
	protected String player;
	protected String world;
	protected String action_type;
	protected String time;
	protected String entity;
	protected String block;
	
	protected int limit = 1000000;
//	protected int offset = 0;
	
	
	/**
	 * @return the entity
	 */
	public String getEntity() {
		return entity;
	}


	/**
	 * @param entity the entity to set
	 */
	public void setEntity(String entity) {
		this.entity = entity;
	}


	/**
	 * @return the block
	 */
	public String getBlock() {
		return block;
	}


	/**
	 * @param block the block to set
	 */
	public void setBlock(String block) {
		this.block = block;
	}
	
	
	/**
	 * @return the loc
	 */
	public Location getLoc() {
		return loc;
	}
	
	
	/**
	 * @param loc the loc to set
	 */
	public void setLoc(Location loc) {
		this.loc = loc;
	}
	
	
	/**
	 * @return the player_location
	 */
	public Vector getPlayer_location() {
		return player_location;
	}


	/**
	 * @param player_location the player_location to set
	 */
	public void setPlayer_location(Vector player_location) {
		this.player_location = player_location;
	}


	/**
	 * @return the radius
	 */
	public int getRadius() {
		return radius;
	}
	
	
	/**
	 * @param radius the radius to set
	 */
	public void setRadius(int radius) {
		this.radius = radius;
	}
	
	
	/**
	 * @return the player
	 */
	public String getPlayer() {
		return player;
	}
	
	
	/**
	 * @param player the player to set
	 */
	public void setPlayer(String player) {
		this.player = player;
	}
	
	
	/**
	 * @return the world
	 */
	public String getWorld() {
		return world;
	}
	
	
	/**
	 * @param world the world to set
	 */
	public void setWorld(String world) {
		this.world = world;
	}
	
	
	/**
	 * @return the action_type
	 */
	public String getAction_type() {
		return action_type;
	}
	
	
	/**
	 * @param action_type the action_type to set
	 */
	public void setAction_type(String action_type) {
		this.action_type = action_type;
	}
	
	
	/**
	 * @return the time
	 */
	public String getTime() {
		return time;
	}
	
	
	/**
	 * @param time the time to set
	 */
	public void setTime(String time) {
		this.time = time;
	}


	/**
	 * @return the limit
	 */
	public int getLimit() {
		return limit;
	}


	/**
	 * @param limit the limit to set
	 */
	public void setLimit(int limit) {
		this.limit = limit;
	}


	/**
	 * @return the lookup_type
	 */
	public String getLookup_type() {
		return lookup_type;
	}


	/**
	 * @param lookup_type the lookup_type to set
	 */
	public void setLookup_type(String lookup_type) {
		this.lookup_type = lookup_type;
	}
	
	
	/**
	 * If we're doing a lookup, we want the most
	 * recent actions available. However, if we're
	 * doing a rollback we want them applied in order.
	 * 
	 * @return
	 */
	public String getSortDirection(){
		if(this.lookup_type.equals("lookup")){
			return "DESC";
		}
		return "ASC";
	}
}
