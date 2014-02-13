package me.botsko.prism.database.mysql;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import me.botsko.elixr.TypeUtils;
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
			// Include IDs
			ArrayList<String> includeIds = new ArrayList<String>();
			for( Entry<String,MatchRule> entry : action_types.entrySet() ){
				if( entry.getValue().equals(MatchRule.INCLUDE) ){
					includeIds.add( ""+Prism.prismActions.get(entry.getKey()) );
				}
			}
			if( includeIds.size() > 0 ){
				addCondition( "action_id IN (" + TypeUtils.join(includeIds, ",")+ ")" );
			}
			// Exclude IDs
			ArrayList<String> excludeIds = new ArrayList<String>();
			for( Entry<String,MatchRule> entry : action_types.entrySet() ){
				if( entry.getValue().equals(MatchRule.EXCLUDE) ){
					excludeIds.add( ""+Prism.prismActions.get(entry.getKey()) );
				}
			}
			if( excludeIds.size() > 0 ){
				addCondition( "action_id NOT IN (" + TypeUtils.join(excludeIds, ",")+ ")" );
			}
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