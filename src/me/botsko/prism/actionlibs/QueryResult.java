package me.botsko.prism.actionlibs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import me.botsko.prism.actions.Action;

public class QueryResult {
	
	/**
	 * 
	 */
	protected List<Action> actionResults = new ArrayList<Action>();
	
	/**
	 * 
	 */
	protected String queryTime;
	
	/**
	 * 
	 */
	protected int per_page;
	
	/**
	 * 
	 */
	protected int page;
	
	
	/**
	 * 
	 * @param actions
	 */
	public QueryResult( List<Action> actions ){
		
		this.actionResults = actions;
		
		java.util.Date date= new java.util.Date();
		queryTime = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		
	}


	/**
	 * @return the actionResults
	 */
	public List<Action> getActionResults() {
		return actionResults;
	}


	/**
	 * @return the queryTime
	 */
	public String getQueryTime() {
		return queryTime;
	}


	/**
	 * @return the per_page
	 */
	public int getPer_page() {
		return per_page;
	}


	/**
	 * @param per_page the per_page to set
	 */
	public void setPer_page(int per_page) {
		this.per_page = per_page;
	}


	/**
	 * @return the page
	 */
	public int getPage() {
		return page;
	}


	/**
	 * @param page the page to set
	 */
	public void setPage(int page) {
		this.page = page;
	}
}