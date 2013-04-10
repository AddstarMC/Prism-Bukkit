package me.botsko.prism.actionlibs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.bukkit.Location;
import org.bukkit.util.Vector;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.appliers.PrismProcessType;


public class QueryBuilder {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private ArrayList<String> columns = new ArrayList<String>();
	
	/**
	 * 
	 */
	private ArrayList<String> conditions = new ArrayList<String>();
	
	/**
	 * 
	 */
	private String tableName = "prism_actions";
	
	
	/**
	 * 
	 * @param plugin
	 */
	public QueryBuilder( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param plugin
	 * @param parameters
	 * @return
	 */
	public String buildQuery( QueryParameters parameters, boolean shouldGroup ){
		
		String dbMode = plugin.getConfig().getString("prism.database.mode");
		
		
		/**
		 * Query prefix and columns
		 */
		String query = "";
		if( !parameters.getProcessType().equals(PrismProcessType.DELETE) ){
			
			query += "SELECT ";
			
			columns.add("id");
			columns.add("action_time");
			columns.add("action_type");
			columns.add("player");
			columns.add("world");
			
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
			
			if( dbMode.equalsIgnoreCase("sqlite") ){
				columns.add("date("+tableName+".action_time) AS display_date");
				columns.add("time("+tableName+".action_time) AS display_time");
			}
			else if( dbMode.equalsIgnoreCase("mysql") ){
				columns.add("DATE_FORMAT("+tableName+".action_time, '%c/%e/%y') AS display_date");
				columns.add("DATE_FORMAT("+tableName+".action_time, '%l:%i:%s%p') AS display_time");
			}
			
			if( shouldGroup ){
				columns.add("COUNT(*) counted");
			}
			
			// Append all columns
			if( columns.size() > 0 ){
				query += TypeUtils.join(columns, ", ");
			}
			
		} else {
			query += "DELETE";
		}
		
		/**
		 * From
		 */
		query += " FROM "+tableName+" ";
		
		/**
		 * ID
		 * 
		 * If we're querying for an ID, none of the other arguments matter.
		 */
		int id = parameters.getId();
		if(id > 0){
			addCondition(tableName+".id = "+id);
		} else {
			
			/**
			 * World
			 */
			if( !parameters.getProcessType().equals(PrismProcessType.DELETE) && parameters.getWorld() != null ){
				addCondition( String.format(tableName+".world = '%s'", parameters.getWorld()) );
			}

			/**
			 * Actions
			 */
			HashMap<String,MatchRule> action_types = parameters.getActionTypeNames();
			
			// Make sure none of the prism process types are requested
			boolean containtsPrismProcessType = false;
			boolean hasPositiveMatchRule = false;
			if( !action_types.isEmpty() ){
				addCondition( buildMultipleConditions( action_types, tableName+".action_type", null ) );
				for (Entry<String,MatchRule> entry : action_types.entrySet()){
					if(entry.getKey().contains("prism")){
						containtsPrismProcessType = true;
						break;
					}
					if(entry.getValue().equals(MatchRule.INCLUDE)){
						hasPositiveMatchRule = true;
					}
				}
			}
			
			if( !containtsPrismProcessType && !parameters.getProcessType().equals(PrismProcessType.DELETE) && !hasPositiveMatchRule ){
				addCondition( tableName+".action_type NOT LIKE '%prism%'" );
			}
			
			/**
			 * Players
			 */
			HashMap<String,MatchRule> playerNames = parameters.getPlayerNames();
			addCondition( buildMultipleConditions( playerNames, tableName+".player", null ) );
			
			/**
			 * Radius
			 * 
			 * Only build a radius condition if we're doing a select. If doing a delete, 
			 * only use the radius if it was specified by the player.
			 */
			if( !parameters.getProcessType().equals(PrismProcessType.DELETE) || (parameters.getProcessType().equals(PrismProcessType.DELETE) && parameters.getFoundArgs().containsKey("r") ) ){
				buildRadiusCondition(parameters.getMinLocation(), parameters.getMaxLocation());
			}

			/**
			 * Block
			 */
			HashMap<Integer,Byte> blockfilters = parameters.getBlockFilters();
			if(!blockfilters.isEmpty()){
				String[] blockArr = new String[blockfilters.size()];
				int i = 0;
				for (Entry<Integer,Byte> entry : blockfilters.entrySet()){
					if( entry.getValue() == 0 ){
						blockArr[i] = tableName+".block_id = " + entry.getKey();
					} else {
						blockArr[i] = tableName+".block_id = " + entry.getKey() + " AND "+tableName+".block_subid = " +  entry.getValue();
					}
					i++;
				}
				addCondition( buildGroupConditions(null, blockArr, "%s%s", "OR", null) );
			}
			
			/**
			 * Entity
			 */
			HashMap<String,MatchRule> entityNames = parameters.getEntities();
			if( entityNames.size() > 0 ){
				addCondition( buildMultipleConditions( entityNames, tableName+".data", "entity_name\":\"%s" ) );
			}
			
			/**
			 * Timeframe
			 */
			String time = parameters.getBeforeTime();
			if(time != null){
				addCondition( buildTimeCondition(time,"<=") );
			}
			time = parameters.getSinceTime();
			if(time != null){
				addCondition( buildTimeCondition(time,null) );
			}
			
			/**
			 * Keywords
			 */
			String keyword = parameters.getKeyword();
			if(keyword != null){
				addCondition( tableName+".data LIKE '%"+keyword+"%'" );
			}
			
			/**
			 * Specific coords
			 */
			ArrayList<Location> locations = parameters.getSpecificBlockLocations();
			if( locations.size() >0 ){
				String coordCond = "(";
				int l = 0;
				for( Location loc : locations ){
					coordCond += (l > 0 ? " OR" : "" ) + " (prism_actions.x = " +(int)loc.getBlockX()+ " AND prism_actions.y = " +(int)loc.getBlockY()+ " AND prism_actions.z = " +(int)loc.getBlockZ() + ")";
					l++;
				}
				coordCond += ")";
				addCondition( coordCond );
			}
			
			
			/**
			 * Parent process id
			 */
			if(parameters.getParentId() > 0){
				addCondition( String.format("data = %d", parameters.getParentId()) );
			}
			
			
			/**
			 * WHERE
			 */
			int condCount = 1;
			if( conditions.size() > 0 ){
				for(String cond : conditions){
					if( condCount == 1 ){
						query += "WHERE ";
					}
					else {
						query += " AND ";
					}
					query += cond;
					condCount++;
				}
			}
			
			
			/**
			 * ORDER, GROUP, LIMIT
			 */
			if(!parameters.getProcessType().equals(PrismProcessType.DELETE)){
				
				// If lookup, determine if we need to group
				// Do it! Or not...
				if( shouldGroup ){
					query += " GROUP BY "+tableName+".action_type, "+tableName+".player, "+tableName+".block_id, "+tableName+".data";
				}
			
				/**
				 * Order by
				 */
				String sort_dir = parameters.getSortDirection();
				query += " ORDER BY "+tableName+".action_time "+sort_dir+", x ASC, z ASC, y ASC, id "+sort_dir;
				
				/**
				 * LIMIT
				 */
				if( parameters.getProcessType().equals(PrismProcessType.LOOKUP) ){
					int limit = parameters.getLimit();
					if(limit > 0){
						query += " LIMIT "+limit;
					}
				}
			} else {
				// Only limit delete records if using mysql or sqlite has delete limits enabled
				if( dbMode.equals("mysql") || plugin.getConfig().getBoolean("prism.sqlite.enable-delete-limit") ){
					int perBatch = plugin.getConfig().getInt("prism.purge.records-per-batch");
					if( perBatch < 100){
						perBatch = 100;
					}
					query += " LIMIT " + perBatch;
				}
			}
		}
		
		query += ";";
		
		if(plugin.getConfig().getBoolean("prism.debug")){
			Prism.debug(query);
		}
		
		return query;
		
	}
	
	
	/**
	 * 
	 * @param condition
	 */
	protected void addCondition( String condition ){
		if( !condition.isEmpty() ){
			conditions.add( condition );
		}
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
	 * @param arg_values
	 * @param player_name
	 * @return
	 */
	protected void buildRadiusCondition( Vector minLoc, Vector maxLoc ){
		if(minLoc != null && maxLoc != null ){
			addCondition( "("+tableName+".x BETWEEN " + minLoc.getBlockX() + " AND " + maxLoc.getBlockX() + ")" );
			addCondition( "("+tableName+".y BETWEEN " + minLoc.getBlockY() + " AND " + maxLoc.getBlockY() + ")" );
			addCondition( "("+tableName+".z BETWEEN " + minLoc.getBlockZ() + " AND " + maxLoc.getBlockZ() + ")" );
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String buildTimeCondition( String dateFrom, String equation ){
		String where = "";
		if(dateFrom != null){
			if(equation == null){
				addCondition( tableName+".action_time >= '" + dateFrom + "'" );
			} else {
				addCondition( tableName+".action_time "+equation+" '" + dateFrom + "'" );
			}
		}
		return where;
	}
}