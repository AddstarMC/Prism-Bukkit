package me.botsko.prism.actiontypes;

public class EntityShearType extends GenericActionType {
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionType(){
		return "entity-shear";
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
		return "sheared";
	}
}