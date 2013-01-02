package me.botsko.prism.actiontypes;

public class GenericActionType implements ActionType {
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionType(){
		return "generic";
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
		return "did something to";
	}
}