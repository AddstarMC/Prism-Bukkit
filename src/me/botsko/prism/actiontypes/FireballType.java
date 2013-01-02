package me.botsko.prism.actiontypes;

public class FireballType extends GenericActionType {
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionType(){
		return "fireball";
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
	public boolean doesCreateBlock(){
		return false;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean canRollback(){
		return false;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean canRestore(){
		return false;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceWordOfAction(){
		return "ignited";
	}
}