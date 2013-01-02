package me.botsko.prism.actions;


public class BaseAction implements Action {

	/**
	 * 
	 */
	protected String action_time;
	
	/**
	 * 
	 */
	protected String display_date;
	
	/**
	 * 
	 */
	protected String display_time;
	
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
	protected String data;

	
	/**
	 * @return the action_time
	 */
	public String getAction_time() {
		return action_time;
	}

	/**
	 * @param action_time the action_time to set
	 */
	public void setAction_time(String action_time) {
		this.action_time = action_time;
	}

	/**
	 * @return the display_date
	 */
	public String getDisplay_date() {
		return display_date;
	}

	/**
	 * @param display_date the display_date to set
	 */
	public void setDisplay_date(String display_date) {
		this.display_date = display_date;
	}

	/**
	 * @return the display_time
	 */
	public String getDisplay_time() {
		return display_time;
	}

	/**
	 * @param display_time the display_time to set
	 */
	public void setDisplay_time(String display_time) {
		this.display_time = display_time;
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
	 * @return the world_name
	 */
	public String getWorld_name() {
		return world_name;
	}

	/**
	 * @param world_name the world_name to set
	 */
	public void setWorld_name(String world_name) {
		this.world_name = world_name;
	}

	/**
	 * @return the player_name
	 */
	public String getPlayer_name() {
		return player_name;
	}

	/**
	 * @param player_name the player_name to set
	 */
	public void setPlayer_name(String player_name) {
		this.player_name = player_name;
	}

	/**
	 * @return the x
	 */
	public double getX() {
		return x;
	}

	/**
	 * @param x the x to set
	 */
	public void setX(double x) {
		this.x = x;
	}

	/**
	 * @return the y
	 */
	public double getY() {
		return y;
	}

	/**
	 * @param y the y to set
	 */
	public void setY(double y) {
		this.y = y;
	}

	/**
	 * @return the z
	 */
	public double getZ() {
		return z;
	}

	/**
	 * @param z the z to set
	 */
	public void setZ(double z) {
		this.z = z;
	}

	/**
	 * @return the data
	 */
	public String getData() {
		return data;
	}

	/**
	 * @param data the data to set
	 */
	public void setData(String data) {
		this.data = data;
	}
	
}
