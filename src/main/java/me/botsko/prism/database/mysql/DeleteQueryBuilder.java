package me.botsko.prism.database.mysql;

import me.botsko.prism.Prism;

public class DeleteQueryBuilder extends SelectQueryBuilder {
	
	
	/**
	 * 
	 * @param plugin
	 */
	public DeleteQueryBuilder( Prism plugin ){
		super(plugin);
	}
	
	
	/**
	 * 
	 */
	@Override
	public String select(){
		return "DELETE FROM " + tableNameData;
	}

	
	/**
	 * 
	 */
	@Override
	protected String group(){
		return "";
	}
	
	
	/**
	 * 
	 */
	@Override
	protected String order(){
		return "";
	}
	
	
	/**
	 * 
	 */
	@Override
	protected String limit(){
		return "";
	}
}