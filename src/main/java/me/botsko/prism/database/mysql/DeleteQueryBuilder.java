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
	protected void worldCondition(){
		if( parameters.getWorld() != null ){
			addCondition( String.format( "world_id = ( SELECT w.world_id FROM prism_worlds w WHERE w.world = '%s')", parameters.getWorld()) );
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
		return "";
	}
}