package me.botsko.prism.actionlibs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import me.botsko.prism.actions.ActionType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;

import org.bukkit.Location;
import org.bukkit.util.Vector;

/**
 * Query Parameters allows you to add values with which Prism
 * will build the database queries.
 * 
 * @author botskonet
 *
 */
public class QueryParameters implements Cloneable {
	
	/**
	 * Internal use
	 */
	protected ConcurrentHashMap<String,String> foundArgs = new ConcurrentHashMap<String,String>();
	protected PrismProcessType processType = PrismProcessType.LOOKUP;
	protected ArrayList<String> defaultsUsed = new ArrayList<String>();
	protected String original_command;
	
	/**
	 * Single-value options
	 */
	protected boolean allow_no_radius = false;
	protected int id = 0;
	protected Vector maxLoc;
	protected Vector minLoc;
	protected int parent_id = 0;
	protected Location player_location;
	protected int radius;
	protected Location specific_block_loc;
	protected String since_time;
	protected String before_time;
	protected String world;
	protected String keyword;

	/**
	 * Params that allow multiple values
	 */
	protected HashMap<ActionType,MatchRule> actionTypeRules = new HashMap<ActionType,MatchRule>();
	protected HashMap<Integer,Byte> block_filters = new HashMap<Integer,Byte>();
	protected HashMap<String,MatchRule> entity_filters = new HashMap<String,MatchRule>();
	protected HashMap<String,MatchRule> player_names = new HashMap<String,MatchRule>();
	protected ArrayList<Flag> flags = new ArrayList<Flag>();
	
	/**
	 * Pagination
	 */
	protected int per_page = 5;
	protected int limit = 1000000;
	

	/**
	 * @return the id
	 */
	public int getId() {
		return id;
	}


	/**
	 * @param id the id to set
	 */
	public void setId(int id) {
		this.id = id;
	}
	
	
	/**
	 * @return the entity
	 */
	public HashMap<String,MatchRule> getEntities() {
		return entity_filters;
	}


	/**
	 * @param entity the entity to set
	 */
	public void addEntity(String entity) {
		addEntity(entity, MatchRule.INCLUDE);
	}
	
	
	/**
	 * @param entity the entity to set
	 */
	public void addEntity( String entity, MatchRule match ) {
		this.entity_filters.put(entity, match);
	}


	/**
	 * @return the block
	 */
	public HashMap<Integer,Byte> getBlockFilters() {
		return block_filters;
	}
	
	
	/**
	 * @param block the block to set
	 */
	public void addBlockFilter( int id, byte data ) {
		this.block_filters.put(id,data);
	}
	
	
	/**
	 * @return the loc
	 */
	public Location getSpecificBlockLocation() {
		return specific_block_loc;
	}
	
	
	/**
	 * @param loc the loc to set
	 */
	public void setSpecificBlockLocation(Location loc) {
		this.specific_block_loc = loc;
	}
	
	
	/**
	 * @return the player_location
	 */
	public Location getPlayerLocation() {
		return player_location;
	}

	
	/**
	 * 
	 * @param loc
	 */
	public void setMinMaxVectorsFromPlayerLocation(Location loc){
		this.player_location = loc;
		if(radius > 0){
			minLoc = new Vector(loc.getX() - radius, loc.getY() - radius, loc.getZ() - radius);
			maxLoc = new Vector(loc.getX() + radius, loc.getY() + radius, loc.getZ() + radius);
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Vector getMinLocation(){
		return minLoc;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public void setMinLocation(Vector minLoc){
		this.minLoc = minLoc;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Vector getMaxLocation(){
		return maxLoc;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public void setMaxLocation(Vector maxLoc){
		this.maxLoc = maxLoc;
	}


	/**
	 * @return the radius
	 */
	public int getRadius() {
		return radius;
	}
	
	
	/**
	 * @param radius the radius to set
	 */
	public void setRadius(int radius) {
		this.radius = radius;
	}
	
	
	/**
	 * @return the allow_no_radius
	 */
	public boolean allowsNoRadius() {
		return allow_no_radius;
	}


	/**
	 * @param allow_no_radius the allow_no_radius to set
	 */
	public void setAllowNoRadius(boolean allow_no_radius) {
		this.allow_no_radius = allow_no_radius;
	}


	/**
	 * @return the player
	 */
	public HashMap<String,MatchRule> getPlayerNames() {
		return player_names;
	}
	
	
	/**
	 * @param player the player to set
	 */
	public void addPlayerName( String player ){
		addPlayerName(player,MatchRule.INCLUDE);
	}
	
	
	/**
	 * @param player the player to set
	 */
	public void addPlayerName( String player, MatchRule match ) {
		this.player_names.put(player,match);
	}
	
	
	/**
	 * @return the world
	 */
	public String getWorld() {
		return world;
	}
	
	
	/**
	 * @param world the world to set
	 */
	public void setWorld(String world) {
		this.world = world;
	}
	
	
	/**
	 * @return the world
	 */
	public String getKeyword() {
		return keyword;
	}
	
	
	/**
	 * @param world the world to set
	 */
	public void setKeyword(String keyword) {
		this.keyword = keyword;
	}
	
	
	/**
	 * @return the action_type
	 */
	public HashMap<ActionType,MatchRule> getActionTypes() {
		return actionTypeRules;
	}
	
	
	/**
	 * @return the action_type
	 */
	public HashMap<String,MatchRule> getActionTypeNames() {
		HashMap<String,MatchRule> types = new HashMap<String,MatchRule>();
		for (Entry<ActionType,MatchRule> entry : actionTypeRules.entrySet()){
			types.put(entry.getKey().getActionType(), entry.getValue());
		}
		return types;
	}
	
	
	/**
	 * @param action_type the action_type to set
	 */
	public void addActionType(ActionType action_type) {
		addActionType(action_type,MatchRule.INCLUDE);
	}
	
	
	/**
	 * @param action_type the action_type to set
	 */
	public void addActionType(ActionType action_type, MatchRule match) {
		this.actionTypeRules.put(action_type,match);
	}
	
	
	/**
	 * @return the action_type
	 */
	public void removeActionType(ActionType a) {
		actionTypeRules.remove(a);
	}
	
	
	/**
	 * 
	 */
	public void resetActionTypes() {
		actionTypeRules.clear();
	}
	
	
	/**
	 * @return the time
	 */
	public String getBeforeTime() {
		return before_time;
	}
	
	
	/**
	 * @param time the time to set
	 */
	public void setBeforeTime(String time) {
		this.before_time = time;
	}
	
	
	/**
	 * @return the time
	 */
	public String getSinceTime() {
		return since_time;
	}
	
	
	/**
	 * @param time the time to set
	 */
	public void setSinceTime(String time) {
		this.since_time = time;
	}


	/**
	 * @return the limit
	 */
	public int getLimit() {
		return limit;
	}


	/**
	 * @param limit the limit to set
	 */
	public void setLimit(int limit) {
		this.limit = limit;
	}


	/**
	 * @return the lookup_type
	 */
	public PrismProcessType getProcessType() {
		return processType;
	}
	
	
	/**
	 * @param lookup_type the lookup_type to set
	 */
	public void setProcessType(PrismProcessType lookup_type) {
		this.processType = lookup_type;
	}


	/**
	 * @return the foundArgs
	 */
	public ConcurrentHashMap<String, String> getFoundArgs() {
		return foundArgs;
	}


	/**
	 * @param foundArgs the foundArgs to set
	 */
	public void setFoundArgs(ConcurrentHashMap<String, String> foundArgs) {
		this.foundArgs = foundArgs;
	}

	
	/**
	 * 
	 * @param id
	 */
	public void setParentId(int id){
		this.parent_id = id;
	}
	
	
	/**
	 * 
	 * @param id
	 */
	public int getParentId(){
		return parent_id;
	}
	
	
	/**
	 * LOOKUP = Most recent actions first.
	 * ROLLBACK = Newest->Oldest so we can "rewind" the events
	 * RESTORE = Oldest->Newest so we can "replay" the events
	 * 
	 * @return
	 */
	public String getSortDirection(){
		if( !this.processType.equals(PrismProcessType.RESTORE) ){
			return "DESC";
		}
		return "ASC";
	}
	
	
	
	/**
	 * 
	 * @return
	 */
	public void addFlag( Flag flag ){
		if(hasFlag(flag)) return;
		this.flags.add(flag);
	}
	
	
	/**
	 * 
	 * @param flag
	 * @return
	 */
	public boolean hasFlag( Flag flag ){
		return flags.contains(flag);
	}
	
	
	/**
	 * 
	 */
	public int getPerPage(){
		return per_page;
	}
	
	
	/**
	 * 
	 * @param per_page
	 */
	public void setPerPage( int per_page ){
		this.per_page = per_page;
	}
	
	
	/**
	 * 
	 * @param d
	 */
	public void addDefaultUsed( String d ){
		defaultsUsed.add(d);
	}
	
	
	/**
	 * 
	 * @return
	 */
	public ArrayList<String> getDefaultsUsed(){
		return defaultsUsed;
	}
	
	
	/**
	 * This just provides easy access to whether or not any action
	 * type we're searching for should also trigger a restore
	 * of any events afterwards.
	 * 
	 * @param at
	 * @return
	 */
	public boolean shouldTriggerRestoreFor( ActionType at ){
		if(!getActionTypes().isEmpty()){
			for (Entry<ActionType,MatchRule> entry : getActionTypes().entrySet()){
				if(entry.getKey().shouldTriggerRestoreFor( at )){
					return true;
				}
			}
		}
		return false;
	}
	
	
	/**
	 * This just provides easy access to whether or not any action
	 * type we're searching for should also trigger a rollback
	 * of any events afterwards.
	 * 
	 * @param at
	 * @return
	 */
	public boolean shouldTriggerRollbackFor( ActionType at ){
		if(!getActionTypes().isEmpty()){
			for (Entry<ActionType,MatchRule> entry : getActionTypes().entrySet()){
				if(entry.getKey().shouldTriggerRollbackFor( at )){
					return true;
				}
			}
		}
		return false;
	}
	
	
	/**
	 * 
	 * @param args
	 */
	public void setStringFromRawArgs( String[] args ){
		String params = "";
		if(args.length > 0){
			for(int i = 0; i < args.length; i++){
				params += " "+args[i];
			}
		}
		original_command = params;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getOriginalCommand(){
		return original_command;
	}
	
	
	/**
	 * 
	 */
	@Override
	public QueryParameters clone() throws CloneNotSupportedException {
		QueryParameters cloned = (QueryParameters) super.clone();
		cloned.actionTypeRules = new HashMap<ActionType,MatchRule>(actionTypeRules);
		return cloned;
	}
}
