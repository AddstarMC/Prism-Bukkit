package me.botsko.prism.database.mysql;

import java.util.HashMap;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;

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
	protected void actionCondition(){
		// Action type
		HashMap<String,MatchRule> action_types = parameters.getActionTypeNames();
		if( action_types.size() > 0 ){
			addCondition( "action_id IN ( SELECT action_id FROM prism_actions WHERE " + buildMultipleConditions( action_types, "action", null ) + ")" );
		}
	}
	
	
	/**
	 * 
	 */
	protected void playerCondition(){
		// Player(s)
		HashMap<String,MatchRule> playerNames = parameters.getPlayerNames();
		if( playerNames.size() > 0 ){
			addCondition( "player_id IN ( SELECT player_id FROM prism_players WHERE " + buildMultipleConditions( playerNames, "player", null ) + ")" );
		}
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
		int perBatch = plugin.getConfig().getInt("prism.purge.records-per-batch");
		if( perBatch < 100){
			perBatch = 100;
		}
		return " LIMIT " + perBatch;
	}
}