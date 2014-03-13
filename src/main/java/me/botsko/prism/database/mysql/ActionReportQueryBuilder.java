package me.botsko.prism.database.mysql;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

public class ActionReportQueryBuilder extends SelectQueryBuilder {
	
	
	/**
	 * 
	 * @param plugin
	 */
	public ActionReportQueryBuilder( Prism plugin ){
		super(plugin);
	}
	
	
	/**
	 * 
	 * @param parameters
	 * @param shouldGroup
	 * @return
	 */
	@Override
	public String getQuery( QueryParameters parameters, boolean shouldGroup ){
		
		this.parameters = parameters;
		this.shouldGroup = shouldGroup;
		
		// Reset
		columns = new ArrayList<String>();
		conditions = new ArrayList<String>();
		
		String query = select();

		query += ";";
		
		if(plugin.getConfig().getBoolean("prism.debug")){
			Prism.debug(query);
		}
		
		return query;
		
	}
	
	
	/**
	 * 
	 */
	@Override
	public String select(){
		
		String sql = "SELECT COUNT(*), a.action "
		+ "FROM prism_data "
		+ "INNER JOIN prism_actions a ON a.action_id = prism_data.action_id "
		+ "INNER JOIN prism_players p ON p.player_id = prism_data.player_id "
		+ where() + " "
		+ "GROUP BY a.action_id "
		+ "ORDER BY COUNT(*) DESC";

		return sql;
		
	}
}