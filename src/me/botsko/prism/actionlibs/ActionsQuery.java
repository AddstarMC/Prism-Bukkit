package me.botsko.prism.actionlibs;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import org.bukkit.Location;
import org.bukkit.entity.Player;
import org.bukkit.util.Vector;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.BlockShiftAction;
import me.botsko.prism.actions.CommandAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.GenericAction;
import me.botsko.prism.actions.GrowAction;
import me.botsko.prism.actions.HangingItemAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.PlayerDeathAction;
import me.botsko.prism.actions.PrismProcessAction;
import me.botsko.prism.actions.PrismRollbackAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.actions.SkullAction;
import me.botsko.prism.actions.UseAction;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;

public class ActionsQuery {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public ActionsQuery(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public QueryResult lookup( Player player, QueryParameters parameters ){
		
		// Pull results
		List<Action> actions = new ArrayList<Action>();
		
		// Build conditions based off final args
		String query = getArgumentConditions(parameters);
		
		if(query!= null){
			try {
	            
				plugin.dbc();
				
	            PreparedStatement s;
	    		s = plugin.conn.prepareStatement(query);
	    		s.executeQuery();
	    		ResultSet rs = s.getResultSet();
	    		
	    		while(rs.next()){
	    			
	    			GenericAction baseAction = null;
	    			boolean override_data = false;
	    			
	    			// Pull the proper action type class
	    			ActionType actionType = ActionType.getByActionType(rs.getString("action_type"));

	    			if(actionType.requiresHandler("block")){
	    				BlockAction b = new BlockAction(null, null, null);
	    				baseAction = b;
	    			}
	    			else if(actionType.requiresHandler("blockshift")){
	    				BlockShiftAction b = new BlockShiftAction(null, null, null, null);
	    				baseAction = b;
	    			}
	    			else if(actionType.requiresHandler("command")){
	    				CommandAction b = new CommandAction(null, null, null, null);
	    				baseAction = b;
	    			}
	    			else if( actionType.requiresHandler("entity") ){
	    				EntityAction eka = new EntityAction(null, null, null);
	    				baseAction = eka;
	    			}
	    			else if( actionType.requiresHandler("grow") ){
	    				GrowAction ga = new GrowAction(null, null, null);
	    				baseAction = ga;
	    			}
	    			else if( actionType.requiresHandler("hangingitem") ){
	    				HangingItemAction ha = new HangingItemAction(null, null, null);
	    				baseAction = ha;
	    			}
	    			else if( actionType.requiresHandler("itemstack") ){
	    				ItemStackAction isa = new ItemStackAction(null, null, 0, null, null);
	    				baseAction = isa;
	    			}
	    			else if( actionType.requiresHandler("playerdeath") ){
	    				PlayerDeathAction pd = new PlayerDeathAction(null, null, null, null);
	    				baseAction = pd;
	    			}
	    			else if( actionType.requiresHandler("prismprocess") ){
	    				PrismProcessAction ps = new PrismProcessAction(null, null, null, null);
	    				baseAction = ps;
	    			}
	    			else if( actionType.requiresHandler("prismrollback") ){
	    				
	    				override_data = true;
	    				
	    				// Get the actual process action
	    				PrismRollbackAction pr = new PrismRollbackAction(null, 0, 0, 0, 0, null, 0);
	    				pr.setData( rs.getString("data") );
	    				
	    				// All we really want is a block action to feed to the world change system
	    				BlockAction b = new BlockAction(null, null, null);
	    				b.setBlockId(pr.getOriginalBlockId());
	    				b.setBlockSubId((byte)pr.getOriginalBlockSubId());
	    				baseAction = b;
	    				
	    			}
	    			else if( actionType.requiresHandler("signchange") ){
	    				SignAction sa = new SignAction(null, null, null, null);
	    				baseAction = sa;
	    			}
	    			else if( actionType.requiresHandler("skull") ){
	    				SkullAction sa = new SkullAction(null, null, null);
	    				baseAction = sa;
	    			}
	    			else if( actionType.requiresHandler("use") ){
	    				UseAction use = new UseAction(null, null, null, null);
	    				baseAction = use;
	    			} else {
	    				plugin.log("Important: Action type '" + rs.getString("action_type") + "' has no official handling class, will be shown as generic." );
	    			}
	    			
	    			if(baseAction == null){
	    				baseAction = new GenericAction();
	    			}
	    				
    				// Set all shared values
    				baseAction.setType( actionType );
    				baseAction.setId( rs.getInt("id") );
    				baseAction.setAction_time( rs.getString("action_time") );
    				baseAction.setDisplay_date( rs.getString("display_date") );
    				baseAction.setDisplay_time( rs.getString("display_time") );
    				baseAction.setWorld_name( rs.getString("world") );
    				baseAction.setPlayer_name( rs.getString("player") );
    				baseAction.setX( rs.getInt("x") );
    				baseAction.setY( rs.getInt("y") );
    				baseAction.setZ( rs.getInt("z") );
    				if(!override_data){
    					baseAction.setData( rs.getString("data") );
    				}
    				baseAction.setMaterialAliases( plugin.getItems() );
    				
    				actions.add(baseAction);
	    			
	    		}
	    		
	    		rs.close();
	    		s.close();
	            plugin.conn.close();
	            
	        } catch (SQLException e) {
	            e.printStackTrace();
	        }
		}
		
		// Build result object
		QueryResult res = new QueryResult( actions, parameters );
		
		// Cache it if we're doing a lookup. Otherwise we don't
		// need a cache.
		if(parameters.getLookup_type().equals(PrismProcessType.LOOKUP)){
			if(plugin.cachedQueries.containsKey(player.getName())){
				plugin.cachedQueries.remove(player.getName());
			}
			plugin.cachedQueries.put(player.getName(), res);
		}
		
		// Return it
		return res;
		
	}
	
	
	/**
	 * 
	 * @param person
	 * @param account_name
	 */
	public PrismProcessAction getPrismProcessRecord( int id ){
		PrismProcessAction process = null;
		try {
            
			plugin.dbc();
            PreparedStatement s;
    		s = plugin.conn.prepareStatement ("SELECT * FROM prism_actions WHERE action_type = 'prism-process' AND id = ?");
    		s.setInt(1, id);
    		s.executeQuery();
    		ResultSet rs = s.getResultSet();

    		if(rs.first()){
    			process = new PrismProcessAction(null, null, null, null);
    			// Set all shared values
    			process.setId( rs.getInt("id") );
    			process.setType( ActionType.getByActionType( rs.getString("action_type") ) );
    			process.setAction_time( rs.getString("action_time") );
//    			process.setDisplay_date( rs.getString("display_date") );
//    			process.setDisplay_time( rs.getString("display_time") );
    			process.setWorld_name( rs.getString("world") );
    			process.setPlayer_name( rs.getString("player") );
    			process.setX( rs.getInt("x") );
    			process.setY( rs.getInt("y") );
    			process.setZ( rs.getInt("z") );
    			process.setData( rs.getString("data") );
			}
    		
    		rs.close();
    		s.close();
            plugin.conn.close();
            
        } catch (SQLException e) {
            e.printStackTrace();
        }
		return process;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public int delete( String beforeDateAlias ){
		int rows_affected = 0;
		String beforeDate = buildTimeCondition(beforeDateAlias,"<");
		if(beforeDate != null && !beforeDate.isEmpty()){
			try {
				String query = "DELETE FROM prism_actions WHERE 1=1" + beforeDate;
				plugin.log("Deleting records prior to " + beforeDate + ": " + query);
				plugin.dbc();
				Statement s = plugin.conn.createStatement ();
				rows_affected = s.executeUpdate (query);
				s.close();
				plugin.conn.close();
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
		return rows_affected;
	}
	
	
	/**
	 * 
	 * @param args
	 */
	public String getArgumentConditions( QueryParameters parameters ){
		
		// Build select
		String query = "SELECT " +
				"prism_actions.id, " +
				"prism_actions.action_time, " +
				"prism_actions.action_type, " +
				"prism_actions.player, " +
				"prism_actions.world, " +
				"prism_actions.x, " +
				"prism_actions.y, " +
				"prism_actions.z, " +
				"prism_actions.data, " +
				"DATE_FORMAT(prism_actions.action_time, '%c/%e/%y') display_date, " +
				"DATE_FORMAT(prism_actions.action_time, '%l:%i%p') display_time " +
				"FROM prism_actions";
		
		// If the user has elected to enforce block override prevention,
		// we'll run join conditions to exclude older records for a specific
		// coordinate so that we don't end up rolling back the first block break
		// at a location when there's a newer one, because no overwrite means
		// any after that first will be skipped.
		if( !parameters.hasFlag(Flag.NO_OVERWRITE) ){
			if( parameters.getLookup_type().equals(PrismProcessType.ROLLBACK) && ( parameters.getActionTypes().contains(ActionType.BLOCK_PLACE) ) ){
				query += " JOIN (" +
							"SELECT action_type, x, y, z, max(action_time) as action_time" +
							" FROM prism_actions WHERE (action_type = 'block-place' OR action_type = 'item-remove')" +
							" GROUP BY action_type, x, y, z) latest" +
							" ON prism_actions.action_time = latest.action_time" +
							" AND prism_actions.x = latest.x" +
							" AND prism_actions.y = latest.y" +
							" AND prism_actions.z = latest.z" +
							" AND prism_actions.action_type = latest.action_type";
			}
			else if( parameters.getLookup_type().equals(PrismProcessType.ROLLBACK) && !parameters.getActionTypes().contains(ActionType.ITEM_REMOVE) ) {
				
				query += " JOIN (" +
						"SELECT"+(parameters.getPlayer() != null ? " player," : "")+" action_type, x, y, z, max(action_time) as action_time" +
						" FROM prism_actions" +
						" GROUP BY action_type, x, y, z"+(parameters.getPlayer() != null ? ", player" : "")+") latest" +
						" ON prism_actions.action_time = latest.action_time" +
						" AND prism_actions.x = latest.x" +
						" AND prism_actions.y = latest.y" +
						" AND prism_actions.z = latest.z" +
						" AND prism_actions.action_type = latest.action_type";
				
						if(parameters.getPlayer() != null){
							query += " AND prism_actions.player = latest.player";
						}
	
			}
		}
		
		// World
		query += " WHERE 1=1";
		
		/**
		 * ID
		 * 
		 * If we're querying for an ID, none of the other arguments matter.
		 */
		int id = parameters.getId();
		if(id > 0){
			query += " AND id = " + id;
		} else {
			
			/**
			 * World
			 */
			if(!parameters.getAllow_no_radius()){
				query += " AND world = '"+parameters.getWorld()+"'";
			}

			/**
			 * Actions
			 */
			ArrayList<ActionType> action_types = parameters.getActionTypes();
			if(!action_types.isEmpty()){
				String[] actions = new String[action_types.size()];
				int i = 0; // @todo Might be a better way to do this.
				for(ActionType type : action_types){
					actions[i] = type.getActionType();
					i++;
				}
				query += buildOrQuery("prism_actions.action_type", actions);
			}
			
			// Make sure none of the prism process types are requested
			boolean containtsPrismProcessType = false;
			for(ActionType aType : action_types){
				if(aType.getActionType().contains("prism")){
					containtsPrismProcessType = true;
					break;
				}
			}
			
			if( !containtsPrismProcessType ){
				query += " AND LEFT(prism_actions.action_type,5) != 'prism'";
			}
			
			/**
			 * Players
			 */
			String player = parameters.getPlayer();
			if(player != null){
				query += buildOrQuery("prism_actions.player", player.split(","));
			}
			
			/**
			 * Radius or Selection
			 */
			query += buildRadiusCondition(parameters.getMinLocation(), parameters.getMaxLocation());

			/**
			 * Block
			 */
			ArrayList<String> blockfilters = parameters.getBlockFilters();
			if(!blockfilters.isEmpty()){
				String[] blockArr = new String[blockfilters.size()];
				blockArr = blockfilters.toArray(blockArr);
				query += buildOrQuery("prism_actions.data", blockArr);
			}
			
			/**
			 * Entity
			 */
			String entity = parameters.getEntity();
			if(entity != null){
				String[] entities = entity.split(",");
				if(entities.length > 0){
					String[] entity_matches = new String[entities.length];
					int c = 0;
					for(String e : entities){
						entity_matches[c] = "entity_name\":\""+e;
						c++;
					}
					query += buildOrLikeQuery("prism_actions.data", entity_matches);
				}
			}
			
			/**
			 * Timeframe
			 */
			String time = parameters.getTime();
			if(time != null){
				query += buildTimeCondition(time,null);
			}
			
			/**
			 * Specific coords
			 */
			Location loc = parameters.getSpecificBlockLocation();
			if(loc != null){
				query += " AND prism_actions.x = " +(int)loc.getBlockX()+ " AND prism_actions.y = " +(int)loc.getBlockY()+ " AND prism_actions.z = " +(int)loc.getBlockZ();
			}
			
			/**
			 * Parent process id
			 */
			if(parameters.getParentId() > 0){
				query += " AND data LIKE '%parent_id\":"+parameters.getParentId()+"}%'";
			}
			
			/**
			 * Order by
			 */
			String sort_dir = parameters.getSortDirection();
			query += " ORDER BY prism_actions.action_time "+sort_dir+ ", x ASC, z ASC, y ASC, id "+sort_dir;
			
			/**
			 * LIMIT
			 */
			int limit = parameters.getLimit();
			if(limit > 0){
				query += " LIMIT 0,"+limit;
			}
		}

		if(plugin.getConfig().getBoolean("prism.debug")){
			plugin.debug("Query conditions: " + query);
		}
		
		return query;
		
	}
	
	
	/**
	 * 
	 * @param fieldname
	 * @param arg_values
	 * @return
	 */
	protected String buildOrQuery( String fieldname, String[] arg_values ){
		String where = "";
		if(arg_values.length > 0){
			where += " AND (";
			int c = 1;
			for(String val : arg_values){
				if(c > 1 && c <= arg_values.length){
					where += " OR ";
				}
				where += fieldname + " = '"+val+"'";
				c++;
			}
			where += ")";
		}
		return where;
	}
	
	
	/**
	 * 
	 * @param fieldname
	 * @param arg_values
	 * @return
	 */
	protected String buildOrLikeQuery( String fieldname, String[] arg_values ){
		String where = "";
		if(arg_values.length > 0){
			where += " AND (";
			int c = 1;
			for(String val : arg_values){
				if(c > 1 && c <= arg_values.length){
					where += " OR ";
				}
				where += fieldname + " LIKE '%"+val+"%'";
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
	protected String buildRadiusCondition( Vector minLoc, Vector maxLoc ){
		String where = "";
		if(minLoc != null && maxLoc != null ){
			where += " AND (prism_actions.x BETWEEN " + minLoc.getX() + " AND " + maxLoc.getX() + ")";
			where += " AND (prism_actions.y BETWEEN " + minLoc.getY() + " AND " + maxLoc.getY() + ")";
			where += " AND (prism_actions.z BETWEEN " + minLoc.getZ() + " AND " + maxLoc.getZ() + ")";
		}
		return where;
	}
	
	
	/**
	 * 
	 * @return
	 */
	protected String buildTimeCondition( String dateFrom, String equation ){
		String where = "";
		if(dateFrom != null){
			if(equation == null){
				where += " AND prism_actions.action_time >= '" + dateFrom + "'";
			} else {
				where += " AND prism_actions.action_time "+equation+" '" + dateFrom + "'";
			}
		}
		return where;
	}
}