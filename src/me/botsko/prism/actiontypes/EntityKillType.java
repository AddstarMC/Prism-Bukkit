package me.botsko.prism.actiontypes;

public class EntityKillType extends GenericActionType {
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionType(){
		return "entity-kill";
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isBlockAction(){
		return false;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isEntityAction(){
		return true;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceWordOfAction(){
		return "killed";
	}
}