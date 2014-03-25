package me.botsko.prism.database.mysql;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.bukkit.Location;
import org.bukkit.util.Vector;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.database.QueryBuilder;


public class SelectQueryBuilder extends QueryBuilder {
	

	/**
	 * 
	 * @param plugin
	 */
	public SelectQueryBuilder( Prism plugin ){
		super(plugin);
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String select(){
		
		String query = "";

		query += "SELECT ";
		
		columns.add("id");
		columns.add("epoch");
		columns.add("action_id");
		columns.add("player");
		columns.add("world_id");
		
		if( shouldGroup ){
			columns.add("AVG(x)");
			columns.add("AVG(y)");
			columns.add("AVG(z)");
		} else {
			columns.add("x");
			columns.add("y");
			columns.add("z");
		}
		
		columns.add("block_id");
		columns.add("block_subid");
		columns.add("old_block_id");
		columns.add("old_block_subid");
		columns.add("data");
		columns.add("te_data"); // MCPC+
		
		if( shouldGroup ){
			columns.add("COUNT(*) counted");
		}
		
		// Append all columns
		if( columns.size() > 0 ){
			query += TypeUtils.join(columns, ", ");
		}
		
		// From
		query += " FROM "+tableNameData+" ";
		
		// Joins
		query += "INNER JOIN prism_players p ON p.player_id = "+tableNameData+".player_id ";
		query += "LEFT JOIN "+tableNameDataExtra+" ex ON ex.data_id = "+tableNameData+".id ";
		
		return query;
		
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String where(){
		
		// ID Condition overrides anything else
		int id = parameters.getId();
		if(id > 0){
			return "WHERE " + tableNameData+".id = "+id;
		}
		
		// id range conditions
		int minId = parameters.getMinPrimaryKey();
		int maxId = parameters.getMaxPrimaryKey();
		if( minId > 0 && maxId > 0 && minId != maxId ){
			addCondition(tableNameData+".id >= "+minId);
			addCondition(tableNameData+".id < "+maxId);
		}
		
		worldCondition();
		actionCondition();
		playerCondition();
		radiusCondition();
		blockCondition();
		entityCondition();
		timeCondition();
		keywordCondition();
		coordinateCondition();
		
		return buildWhereConditions();
		
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
	protected void actionCondition(){
		// Action type
		HashMap<String,MatchRule> action_types = parameters.getActionTypeNames();
		boolean containsPrismProcessType = false;

		// Build IDs for prism process actions
		ArrayList<String> prismActionIds = new ArrayList<String>();
		for( Entry<String,Integer> entry : Prism.prismActions.entrySet() ){
			if(entry.getKey().contains("prism")){
				containsPrismProcessType = true;
				prismActionIds.add( ""+Prism.prismActions.get(entry.getKey()) );
			}
		}
		
		// scan whitelist of given actions
		if( action_types.size() > 0 ){
			
			ArrayList<String> includeIds = new ArrayList<String>();
			ArrayList<String> excludeIds = new ArrayList<String>();
			for( Entry<String,MatchRule> entry : action_types.entrySet() ){
				if( entry.getValue().equals(MatchRule.INCLUDE) ){
					includeIds.add( ""+Prism.prismActions.get(entry.getKey()) );
				}
				if( entry.getValue().equals(MatchRule.EXCLUDE) ){
					excludeIds.add( ""+Prism.prismActions.get(entry.getKey()) );
				}
			}
			// Include IDs
			if( includeIds.size() > 0 ){
				addCondition( "action_id IN (" + TypeUtils.join(includeIds, ",")+ ")" );
			}
			// Exclude IDs
			if( excludeIds.size() > 0 ){
				addCondition( "action_id NOT IN (" + TypeUtils.join(excludeIds, ",")+ ")" );
			}
		} else {
			// exclude internal stuff
			if( !containsPrismProcessType && !parameters.getProcessType().equals(PrismProcessType.DELETE) ){
				addCondition( "action_id NOT IN (" + TypeUtils.join(prismActionIds, ",")+ ")" );
			}
		}
	}
	
	
	/**
	 * 
	 */
	protected void playerCondition(){
		HashMap<String,MatchRule> playerNames = parameters.getPlayerNames();
		if( playerNames.size() > 0 ){
			
			// Match the first rule, this needs to change, we can't include and exclude at the same time
			MatchRule playerMatch = MatchRule.INCLUDE;
			for( MatchRule match : playerNames.values() ){
				playerMatch = match;
				break;
			}
			String matchQuery = (playerMatch.equals(MatchRule.INCLUDE)?"IN":"NOT IN");
			addCondition( tableNameData+".player_id "+matchQuery+" ( SELECT p.player_id FROM prism_players p WHERE " + buildMultipleConditions( playerNames, "p.player", null ) + ")" );
		}
	}
	
	
	/**
	 * 
	 */
	protected void radiusCondition(){
		buildRadiusCondition(parameters.getMinLocation(), parameters.getMaxLocation());
	}
	
	
	/**
	 * 
	 */
	protected void blockCondition(){
		// Blocks
		HashMap<Integer,Byte> blockfilters = parameters.getBlockFilters();
		if(!blockfilters.isEmpty()){
			String[] blockArr = new String[blockfilters.size()];
			int i = 0;
			for (Entry<Integer,Byte> entry : blockfilters.entrySet()){
				if( entry.getValue() == 0 ){
					blockArr[i] = tableNameData+".block_id = " + entry.getKey();
				} else {
					blockArr[i] = tableNameData+".block_id = " + entry.getKey() + " AND "+tableNameData+".block_subid = " +  entry.getValue();
				}
				i++;
			}
			addCondition( buildGroupConditions(null, blockArr, "%s%s", "OR", null) );
		}
	}
	
	
	/**
	 * 
	 */
	protected void entityCondition(){
		// Entity
		HashMap<String,MatchRule> entityNames = parameters.getEntities();
		if( entityNames.size() > 0 ){
			addCondition( buildMultipleConditions( entityNames, "ex.data", "entity_name\":\"%s" ) );
		}
	}
	
	
	/**
	 * 
	 */
	protected void timeCondition(){
		// Timeframe
		Long time = parameters.getBeforeTime();
		if( time != null && time != 0 ){
			addCondition( buildTimeCondition(time,"<=") );
		}
		time = parameters.getSinceTime();
		if( time != null && time != 0 ){
			addCondition( buildTimeCondition(time,null) );
		}
	}
	
	
	/**
	 * 
	 */
	protected void keywordCondition(){
		// Keyword(s)
		String keyword = parameters.getKeyword();
		if(keyword != null){
			addCondition( "ex.data LIKE '%"+keyword+"%'" );
		}
	}
	
	
	/**
	 * 
	 */
	protected void coordinateCondition(){
		// Specific coords
		ArrayList<Location> locations = parameters.getSpecificBlockLocations();
		if( locations.size() >0 ){
			String coordCond = "(";
			int l = 0;
			for( Location loc : locations ){
				coordCond += (l > 0 ? " OR" : "" ) + " ("+tableNameData+".x = " + loc.getBlockX() + " AND "+tableNameData+".y = " + loc.getBlockY() + " AND "+tableNameData+".z = " + loc.getBlockZ() + ")";
				l++;
			}
			coordCond += ")";
			addCondition( coordCond );
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String buildWhereConditions(){
		
		// Parent process
//		if(parameters.getParentId() > 0){
//			addCondition( String.format("ex.data = %d", parameters.getParentId()) );
//		}

		// Build final condition string
		int condCount = 1;
		String query = "";
		if( conditions.size() > 0 ){
			for(String cond : conditions){
				if( condCount == 1 ){
					query += " WHERE ";
				}
				else {
					query += " AND ";
				}
				query += cond;
				condCount++;
			}
		}
		
		return query;
		
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String group(){
		if( shouldGroup ){
			return " GROUP BY "+tableNameData+".action_id, "+tableNameData+".player_id, "+tableNameData+".block_id, ex.data, DATE(FROM_UNIXTIME("+tableNameData+".epoch))";
		}
		return "";
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String order(){
		String sort_dir = parameters.getSortDirection();
		return " ORDER BY "+tableNameData+".epoch "+sort_dir+", x ASC, z ASC, y ASC, id "+sort_dir;	
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String limit(){
		if( parameters.getProcessType().equals(PrismProcessType.LOOKUP) ){
			int limit = parameters.getLimit();
			if(limit > 0){
				return " LIMIT "+limit;
			}
		}
		return "";
	}
	

	
	/**
	 * 
	 * @param origValues
	 * @param field_name
	 * @return
	 */
	protected String buildMultipleConditions( HashMap<String,MatchRule> origValues, String field_name, String format ){
		String query = "";
		if(!origValues.isEmpty()){
			
			ArrayList<String> whereIs = new ArrayList<String>();
			ArrayList<String> whereNot = new ArrayList<String>();
			ArrayList<String> whereIsLike = new ArrayList<String>();
			for (Entry<String,MatchRule> entry : origValues.entrySet()){
				if(entry.getValue().equals(MatchRule.EXCLUDE)){
					whereNot.add(entry.getKey());
				}
				else if(entry.getValue().equals(MatchRule.PARTIAL)){
					whereIsLike.add(entry.getKey());
				} else {
					whereIs.add(entry.getKey());
				}
			}
			// To match
			if(!whereIs.isEmpty()){
				String[] whereValues = new String[whereIs.size()];
				whereValues = whereIs.toArray(whereValues);
				if(format == null){
					query += buildGroupConditions(field_name, whereValues, "%s = '%s'", "OR", null);
				} else {
					query += buildGroupConditions(field_name, whereValues, "%s LIKE '%%%s%%'", "OR", format);
				}
			}
			// To match partial
			if(!whereIsLike.isEmpty()){
				String[] whereValues = new String[whereIsLike.size()];
				whereValues = whereIsLike.toArray(whereValues);
				query += buildGroupConditions(field_name, whereValues, "%s LIKE '%%%s%%'", "OR", format);
			}
			// Not match
			if(!whereNot.isEmpty()){
				String[] whereNotValues = new String[whereNot.size()];
				whereNotValues = whereNot.toArray(whereNotValues);
				
				if(format == null){
					query += buildGroupConditions(field_name, whereNotValues, "%s != '%s'", null, null);
				} else {
					query += buildGroupConditions(field_name, whereNotValues, "%s NOT LIKE '%%%s%%'", null, format);
				}
			}
		}
		return query;
	}
	
	
	/**
	 * 
	 * @param fieldname
	 * @param arg_values
	 * @return
	 */
	protected String buildGroupConditions( String fieldname, String[] arg_values, String matchFormat, String matchType, String dataFormat ){
		
		String where = "";
		matchFormat = (matchFormat == null ? "%s = %s" : matchFormat);
		matchType = (matchType == null ? "AND" : matchType);
		dataFormat = (dataFormat == null ? "%s" : dataFormat);

		if( arg_values.length > 0 && !matchFormat.isEmpty() ){
			where += "(";
			int c = 1;
			for(String val : arg_values){
				if(c > 1 && c <= arg_values.length){
					where += " "+matchType+" ";
				}
				fieldname = ( fieldname == null ? "" : fieldname );
				where += String.format(matchFormat, fieldname, String.format(dataFormat,val));
				c++;
			}
			where += ")";
		}
		return where;
	}
	
	
	/**
	 * 
	 * @param minLoc
     * @param maxLoc
	 * @return
	 */
	protected void buildRadiusCondition( Vector minLoc, Vector maxLoc ){
		if(minLoc != null && maxLoc != null ){
			addCondition( "("+tableNameData+".x BETWEEN " + minLoc.getBlockX() + " AND " + maxLoc.getBlockX() + ")" );
			addCondition( "("+tableNameData+".y BETWEEN " + minLoc.getBlockY() + " AND " + maxLoc.getBlockY() + ")" );
			addCondition( "("+tableNameData+".z BETWEEN " + minLoc.getBlockZ() + " AND " + maxLoc.getBlockZ() + ")" );
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String buildTimeCondition( Long dateFrom, String equation ){
		String where = "";
		if(dateFrom != null){
			if(equation == null){
				addCondition( tableNameData+".epoch >= " + (dateFrom/1000) + "" );
			} else {
				addCondition( tableNameData+".epoch "+equation+" '" + (dateFrom/1000) + "'" );
			}
		}
		return where;
	}
}