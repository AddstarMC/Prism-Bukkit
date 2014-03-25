package me.botsko.prism.actionlibs;

import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;
import org.bukkit.Location;
import org.bukkit.command.CommandSender;
import org.bukkit.util.Vector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

/**
 * Query Parameters allows you to add values with which Prism will build the
 * database queries.
 * 
 * @author botskonet
 * 
 */
public class QueryParameters implements Cloneable {

    /**
     * Internal use
     */
    protected Set<String> foundArgs = new HashSet<String>();
    protected PrismProcessType processType = PrismProcessType.LOOKUP;
    protected final ArrayList<String> defaultsUsed = new ArrayList<String>();
    protected String original_command;

    /**
     * Single-value options
     */
    protected boolean allow_no_radius = false;
    protected int id = 0;
    protected int minId = 0;
    protected int maxId = 0;
    protected Vector maxLoc;
    protected Vector minLoc;
    protected int parent_id = 0;
    protected Location player_location;
    protected int radius;
    protected final ArrayList<Location> specific_block_locations = new ArrayList<Location>();
    protected Long since_time;
    protected Long before_time;
    protected String world;
    protected String keyword;
    protected boolean ignoreTime;

    /**
     * Params that allow multiple values
     */
    protected HashMap<String, MatchRule> actionTypeRules = new HashMap<String, MatchRule>();
    protected final HashMap<Integer, Byte> block_filters = new HashMap<Integer, Byte>();
    protected final HashMap<String, MatchRule> entity_filters = new HashMap<String, MatchRule>();
    protected final HashMap<String, MatchRule> player_names = new HashMap<String, MatchRule>();
    protected final ArrayList<Flag> flags = new ArrayList<Flag>();
    protected final ArrayList<CommandSender> shared_players = new ArrayList<CommandSender>();

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
     * @param id
     *            the id to set
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * 
     * @param minId
     */
    public void setMinPrimaryKey(int minId) {
        this.minId = minId;
    }

    /**
     * 
     * @param minId
     */
    public int getMinPrimaryKey() {
        return this.minId;
    }

    /**
     * 
     * @param maxId
     */
    public void setMaxPrimaryKey(int maxId) {
        this.maxId = maxId;
    }

    /**
     * 
     * @param minId
     */
    public int getMaxPrimaryKey() {
        return this.maxId;
    }

    /**
     * @return the entity
     */
    public HashMap<String, MatchRule> getEntities() {
        return entity_filters;
    }

    /**
     * @param entity
     *            the entity to set
     */
    public void addEntity(String entity) {
        addEntity( entity, MatchRule.INCLUDE );
    }

    /**
     * @param entity
     *            the entity to set
     */
    public void addEntity(String entity, MatchRule match) {
        this.entity_filters.put( entity, match );
    }

    /**
     * @return the block
     */
    public HashMap<Integer, Byte> getBlockFilters() {
        return block_filters;
    }

    /**
     * @param id
     *            the block to set
     */
    public void addBlockFilter(int id, byte data) {
        this.block_filters.put( id, data );
    }

    /**
     * @return the loc
     */
    public ArrayList<Location> getSpecificBlockLocations() {
        return specific_block_locations;
    }

    /*
     * @param loc the loc to set
     */
    public void setSpecificBlockLocation(Location loc) {
        this.specific_block_locations.clear();
        addSpecificBlockLocation( loc );
    }

    /**
     * @param loc
     *            the loc to set
     */
    public void addSpecificBlockLocation(Location loc) {
        this.specific_block_locations.add( loc );
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
    public void setMinMaxVectorsFromPlayerLocation(Location loc) {
        this.player_location = loc;
        if( radius > 0 ) {
            minLoc = new Vector( loc.getX() - radius, loc.getY() - radius, loc.getZ() - radius );
            maxLoc = new Vector( loc.getX() + radius, loc.getY() + radius, loc.getZ() + radius );
        }
    }

    /**
	 * 
	 */
    public void resetMinMaxVectors() {
        minLoc = null;
        maxLoc = null;
    }

    /**
     * 
     * @return
     */
    public Vector getMinLocation() {
        return minLoc;
    }

    /**
     * 
     * @return
     */
    public void setMinLocation(Vector minLoc) {
        this.minLoc = minLoc;
    }

    /**
     * 
     * @return
     */
    public Vector getMaxLocation() {
        return maxLoc;
    }

    /**
     * 
     * @return
     */
    public void setMaxLocation(Vector maxLoc) {
        this.maxLoc = maxLoc;
    }

    /**
     * @return the radius
     */
    public int getRadius() {
        return radius;
    }

    /**
     * @param radius
     *            the radius to set
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
     * @param allow_no_radius
     *            the allow_no_radius to set
     */
    public void setAllowNoRadius(boolean allow_no_radius) {
        this.allow_no_radius = allow_no_radius;
    }

    /**
     * @return the player
     */
    public HashMap<String, MatchRule> getPlayerNames() {
        return player_names;
    }

    /**
     * @param player
     *            the player to set
     */
    public void addPlayerName(String player) {
        addPlayerName( player, MatchRule.INCLUDE );
    }

    /**
     * @param player
     *            the player to set
     */
    public void addPlayerName(String player, MatchRule match) {
        this.player_names.put( player, match );
    }

    /**
     * @return the world
     */
    public String getWorld() {
        return world;
    }

    /**
     * @param world
     *            the world to set
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
     * @param keyword
     *            the world to set
     */
    public void setKeyword(String keyword) {
        this.keyword = keyword;
    }

    /**
     * @return the action_type
     */
    public HashMap<String, MatchRule> getActionTypes() {
        return actionTypeRules;
    }

    /**
     * @return the action_type
     */
    public HashMap<String, MatchRule> getActionTypeNames() {
        return actionTypeRules;
    }

    /**
     * @param action_type
     *            the action_type to set
     */
    public void addActionType(String action_type) {
        addActionType( action_type, MatchRule.INCLUDE );
    }

    /**
     * @param action_type
     *            the action_type to set
     */
    public void addActionType(String action_type, MatchRule match) {
        this.actionTypeRules.put( action_type, match );
    }

    /**
     * @return the action_type
     */
    public void removeActionType(ActionType a) {
        actionTypeRules.remove( a.getName() );
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
    public Long getBeforeTime() {
        return before_time;
    }

    /**
     * @param time
     *            the time to set
     */
    public void setBeforeTime(Long epoch) {
        this.before_time = epoch;
    }

    /**
     * @return the time
     */
    public Long getSinceTime() {
        return since_time;
    }

    /**
     * @param time
     *            the time to set
     */
    public void setSinceTime(Long epoch) {
        this.since_time = epoch;
    }

    /**
     * @return the limit
     */
    public int getLimit() {
        return limit;
    }

    /**
     * @param limit
     *            the limit to set
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
     * @param lookup_type
     *            the lookup_type to set
     */
    public void setProcessType(PrismProcessType lookup_type) {
        this.processType = lookup_type;
    }

    /**
     * @return the foundArgs
     */
    public Set<String> getFoundArgs() {
        return foundArgs;
    }

    /**
     * @param foundArgs
     *            the foundArgs to set
     */
    public void setFoundArgs(Set<String> foundArgs) {
        this.foundArgs = foundArgs;
    }

    /**
     * 
     * @param id
     */
    public void setParentId(int id) {
        this.parent_id = id;
    }

    /**
	 *
	 */
    public int getParentId() {
        return parent_id;
    }

    /**
     * LOOKUP = Most recent actions first. ROLLBACK = Newest->Oldest so we can
     * "rewind" the events RESTORE = Oldest->Newest so we can "replay" the
     * events
     * 
     * @return
     */
    public String getSortDirection() {
        if( !this.processType.equals( PrismProcessType.RESTORE ) ) { return "DESC"; }
        return "ASC";
    }

    /**
     * 
     * @return
     */
    public void addFlag(Flag flag) {
        if( hasFlag( flag ) )
            return;
        this.flags.add( flag );
    }

    /**
     * 
     * @param flag
     * @return
     */
    public boolean hasFlag(Flag flag) {
        return flags.contains( flag );
    }

    /**
	 * 
	 */
    public int getPerPage() {
        return per_page;
    }

    /**
     * 
     * @param per_page
     */
    public void setPerPage(int per_page) {
        this.per_page = per_page;
    }

    /**
     * 
     * @param d
     */
    public void addDefaultUsed(String d) {
        defaultsUsed.add( d );
    }

    /**
     * 
     * @return
     */
    public ArrayList<String> getDefaultsUsed() {
        return defaultsUsed;
    }

    /**
     * 
     * @param args
     */
    public void setStringFromRawArgs(String[] args, int start) {
        String params = "";
        if( args.length > 0 ) {
            for ( int i = start; i < args.length; i++ ) {
                params += " " + args[i];
            }
        }
        original_command = params;
    }

    /**
     * 
     * @return
     */
    public String getOriginalCommand() {
        return original_command;
    }

    /**
     * Get the players that you're sharing your lookup with.
     * 
     * @return
     */
    public ArrayList<CommandSender> getSharedPlayers() {
        return shared_players;
    }

    /**
     * Set the players you're sharing the lookup with.
     * 
     * @param sender
     */
    public void addSharedPlayer(CommandSender sender) {
        this.shared_players.add( sender );
    }

    /**
	 * 
	 */
    @Override
    public QueryParameters clone() throws CloneNotSupportedException {
        final QueryParameters cloned = (QueryParameters) super.clone();
        cloned.actionTypeRules = new HashMap<String, MatchRule>( actionTypeRules );
        return cloned;
    }

    /**
     * Ignore the time.
     * 
     * @param ignore
     */
    public void setIgnoreTime(boolean ignore) {
        this.ignoreTime = ignore;
    }

    /**
     * Check if we are ignoring the time.
     * 
     * @return
     */
    public boolean getIgnoreTime() {
        return ignoreTime;
    }
}
