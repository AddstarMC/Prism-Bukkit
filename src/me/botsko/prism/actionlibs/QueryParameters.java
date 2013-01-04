package me.botsko.prism.actionlibs;

import java.util.ArrayList;
import java.util.HashMap;

import me.botsko.prism.actions.ActionType;

import org.bukkit.Location;
import org.bukkit.util.Vector;

public class QueryParameters {
	
	protected HashMap<String,String> foundArgs = new HashMap<String,String>();
	protected String lookup_type = "lookup";
	
	protected Location loc;
	protected Vector player_location;
	
	protected int id = 0;
	protected int radius;
	protected boolean allow_no_radius = false;
	protected String player;
	protected String world;
	protected ArrayList<ActionType> action_types = new ArrayList<ActionType>();
	protected String time;
	protected String entity;
	protected String block;
	
	protected int limit = 1000000;
//	protected int offset = 0;
	
	
	/**
	 * @return the id
	 */
	public int getId() {
		return id;
	}


	/**
	 * @param id the id to set
	 */
	public void setId(int id) {
		this.id = id;
	}
	
	
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
	 * @return the allow_no_radius
	 */
	public boolean getAllow_no_radius() {
		return allow_no_radius;
	}


	/**
	 * @param allow_no_radius the allow_no_radius to set
	 */
	public void setAllow_no_radius(boolean allow_no_radius) {
		this.allow_no_radius = allow_no_radius;
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
	public ArrayList<ActionType> getActionTypes() {
		return action_types;
	}
	
	
	/**
	 * @param action_type the action_type to set
	 */
	public void addActionType(ActionType action_type) {
		this.action_types.add(action_type);
	}
	
	
	/**
	 * 
	 */
	public void resetActionTypes() {
		action_types.clear();
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
	 * @return the foundArgs
	 */
	public HashMap<String, String> getFoundArgs() {
		return foundArgs;
	}


	/**
	 * @param foundArgs the foundArgs to set
	 */
	public void setFoundArgs(HashMap<String, String> foundArgs) {
		this.foundArgs = foundArgs;
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
