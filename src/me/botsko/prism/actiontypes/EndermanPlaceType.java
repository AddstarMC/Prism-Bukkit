package me.botsko.prism.actiontypes;

public class EndermanPlaceType extends GenericActionType {
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionType(){
		return "enderman-place";
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean doesCreateBlock(){
		return true;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceWordOfAction(){
		return "placed";
	}
}