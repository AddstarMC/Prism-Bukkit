package me.botsko.prism.actions;

import me.botsko.prism.Prism;
import me.botsko.prism.appliers.PrismProcessType;


public class PrismProcessAction extends GenericAction {
	
	public class PrismProcessActionData {
		public String params = "";
		public String processType;
	}
	
	/**
	 * 
	 */
	private PrismProcessActionData actionData;
	
	
	/**
	 * 
	 * @param processType
     * @param parameters
	 */
	public void setProcessData( PrismProcessType processType, String parameters ){
		
		actionData = new PrismProcessActionData();
		
		if(processType != null){
			actionData.params = parameters;
			actionData.processType = processType.name().toLowerCase();
		}
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		if(data != null){
			actionData = gson.fromJson(data, PrismProcessActionData.class);
		}
	}
	
	
	/**
	 * 
	 */
	public void save(){
		data = gson.toJson(actionData);
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getProcessChildActionType(){
		return Prism.getActionRegistry().getAction( "prism-"+actionData.processType ).getName();
	}
	
	
	/**
	 * 
	 */
	public String getNiceName(){
		return actionData.processType + " ("+actionData.params+")";
	}
}