package me.botsko.prism.actions;

public interface Action {
	
	
	/**
	 * 
	 * @return
	 */
	public String getAction_time();
	
	
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
	
	
	/**
	 * 
	 * @return
	 */
	public String getData();

	
	/**
	 * 
	 * @return
	 */
	public String getDisplay_date();
	
}