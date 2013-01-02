package me.botsko.prism.actiontypes;

public interface ActionType {

	
	/**
	 * 
	 * @return
	 */
	public String getActionType();
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isBlockAction();
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isEntityAction();
	
	
	/**
	 * 
	 * @return
	 */
	public boolean doesCreateBlock();
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceWordOfAction();
}
