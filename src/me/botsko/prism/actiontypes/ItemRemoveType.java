package me.botsko.prism.actiontypes;

public class ItemRemoveType extends GenericActionType {
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionType(){
		return "item-remove";
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
	public boolean isItemStackAction(){
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
		return "removed";
	}
}