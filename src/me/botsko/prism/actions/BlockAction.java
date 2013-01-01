package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

public class BlockAction implements Action {
	
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
	protected int block_id;
	
	/**
	 * 
	 */
	protected byte block_subid;
	
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
	 * @param world_name
	 * @param player_name
	 * @param block_id
	 * @param block_subid
	 * @param x
	 * @param y
	 * @param z
	 */
	public BlockAction( String action_type, String world_name, String player_name, int block_id, byte block_subid, double x, double y, double z ){
		
		java.util.Date date= new java.util.Date();
		action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		
		this.action_type = action_type;
		this.world_name = world_name;
		this.player_name = player_name;
		this.block_id = block_id;
		this.block_subid = block_subid;
		this.x = x;
		this.y = y;
		this.z = z;
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
	 * @return the block_id
	 */
	public int getBlock_id() {
		return block_id;
	}

	
	/**
	 * @return the block_subid
	 */
	public int getBlock_subid() {
		return block_subid;
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
}