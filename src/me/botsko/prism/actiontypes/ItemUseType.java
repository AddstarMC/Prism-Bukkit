package me.botsko.prism.actiontypes;

public class ItemUseType extends GenericActionType {
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionType(){
		return "item-use";
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isBlockAction(){
		return true;
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
		return "used";
	}
}