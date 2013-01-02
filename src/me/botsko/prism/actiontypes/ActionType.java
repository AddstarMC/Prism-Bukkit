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
	public boolean doesCreateBlock();
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceWordOfAction();
}
