package me.botsko.prism.actions;

public interface Action {
	
	public String getAction_time();
	public int getBlock_id();
	public int getBlock_subid();
	
	
	/**
	 * 
	 * @return
	 */
	public String getAction_type();
	
	
	/**
	 * 
	 * @return
	 */
	public String getWorld_name();
	
	
	/**
	 * 
	 * @return
	 */
	public String getPlayer_name();
	
	
	/**
	 * 
	 * @return
	 */
	public double getX();
	
	
	/**
	 * 
	 * @return
	 */
	public double getY();
	
	
	/**
	 * 
	 * @return
	 */
	public double getZ();
	

}