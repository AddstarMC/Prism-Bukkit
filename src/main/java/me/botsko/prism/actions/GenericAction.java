package me.botsko.prism.actions;

import java.text.SimpleDateFormat;
import java.util.Date;

import me.botsko.elixr.MaterialAliases;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;

import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.craftbukkit.libs.com.google.gson.Gson;
import org.bukkit.craftbukkit.libs.com.google.gson.GsonBuilder;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;


public class GenericAction implements Handler {
	
	/**
	 * 
	 */
	protected Plugin plugin;
	
	/**
	 * 
	 */
	protected boolean canceled = false;
	
	/**
	 * 
	 */
	protected Gson gson = new GsonBuilder().disableHtmlEscaping().create();
	
	/**
	 * 
	 */
	protected ActionType type;
	
	/**
	 * 
	 */
	protected MaterialAliases materialAliases;
	
	/**
	 * 
	 */
	protected int id;

	/**
	 * 
	 */
	protected String epoch;
	
	/**
	 * 
	 */
	protected String display_date;
	
	/**
	 * 
	 */
	protected String display_time;
	
	/**
	 * 
	 */
	protected String world_name;
	
	/**
	 * 
	 */
	protected String player_name;
	
	/**
	 * 
	 */
	protected double x;
	
	/**
	 * 
	 */
	protected double y;
	
	/**
	 * 
	 */
	protected double z;
	
	/**
	 * 
	 */
	protected int block_id;
	
	/**
	 * 
	 */
	protected int block_subid;
	
	/**
	 * 
	 */
	protected int old_block_id;
	
	/**
	 * 
	 */
	protected int old_block_subid;
	
	/**
	 * 
	 */
	protected String data;

	 // MCPC+ start
	/**
	 * 
	 */
	 protected String te_data;
	 // MCPC+ end

	/**
	 * 
	 */
	protected int aggregateCount = 0;
	
	
	/**
	 * 
	 */
	public void setPlugin( Plugin plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param action_type
	 */
	public void setActionType(String action_type){
		if(action_type != null){
			this.type = Prism.getActionRegistry().getAction( action_type );
		}
	}


	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getId()
	 */
	public int getId() {
		return id;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setId(int)
	 */
	public void setId(int id) {
		this.id = id;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getActionTime()
	 */
	public String getUnixEpoch() {
		return epoch;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getDisplayDate()
	 */
	public String getDisplayDate() {
		return display_date;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setDisplayDate(java.lang.String)
	 */
	public void setUnixEpoch( String epoch ) {
		
		this.epoch = epoch;
		
		Date action_time = new Date(Long.parseLong(epoch) * 1000);
		
		SimpleDateFormat date = new SimpleDateFormat("yy/MM/dd");
		this.display_date = date.format(action_time);
		
		SimpleDateFormat time = new SimpleDateFormat("h:m:sa");
		this.display_time = time.format(action_time);

	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getDisplayTime()
	 */
	public String getDisplayTime() {
		return display_time;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getTimeSince()
	 */
	public String getTimeSince(){
		
		String time_ago = "";
		
		Date start = new Date(Long.parseLong(this.epoch) * 1000);
		Date end = new Date();
		
		long diffInSeconds = (end.getTime() - start.getTime()) / 1000;

	    long diff[] = new long[] { 0, 0, 0, 0 };
	    /* sec */	diff[3] = (diffInSeconds >= 60 ? diffInSeconds % 60 : diffInSeconds);
	    /* min */	diff[2] = (diffInSeconds = (diffInSeconds / 60)) >= 60 ? diffInSeconds % 60 : diffInSeconds;
	    /* hours */	diff[1] = (diffInSeconds = (diffInSeconds / 60)) >= 24 ? diffInSeconds % 24 : diffInSeconds;
	    /* days */	diff[0] = (diffInSeconds = (diffInSeconds / 24));

	    // Only show days if more than 1
	    if(diff[0] >= 1){
	    	time_ago += diff[0] + "d";
	    }
	    // Only show hours if > 1
	    if(diff[1] >= 1){
	    	time_ago += diff[1] + "h";
	    }
	    // Only show minutes if > 1 and less than 60
	    if(diff[2] > 1 && diff[2] < 60){
	    	time_ago += diff[2] + "m";
	    }
	    if(!time_ago.isEmpty()){
	    	time_ago += " ago";
	    }
	    
	    if( diff[0] == 0 && diff[1] == 0 && diff[2] <= 1){
	    	time_ago = "just now";
	    }
		    
		return time_ago;
		
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getType()
	 */
	public ActionType getType() {
		return type;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setType(me.botsko.prism.actionlibs.ActionType)
	 */
	public void setType( ActionType type ){
		this.type = type;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getWorldName()
	 */
	public String getWorldName() {
		return world_name;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setWorldName(java.lang.String)
	 */
	public void setWorldName(String world_name) {
		this.world_name = world_name;
	}
	
	
	/**
	 * 
	 * @param player
	 */
	public void setPlayerName( Player player ){
		if(player != null){
			this.player_name = player.getName();
		}
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getPlayerName()
	 */
	public String getPlayerName() {
		return player_name;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setPlayerName(java.lang.String)
	 */
	public void setPlayerName(String player_name) {
		this.player_name = player_name;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getX()
	 */
	public double getX() {
		return x;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setX(double)
	 */
	public void setX(double x) {
		this.x = x;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getY()
	 */
	public double getY() {
		return y;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setY(double)
	 */
	public void setY(double y) {
		this.y = y;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getZ()
	 */
	public double getZ() {
		return z;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setZ(double)
	 */
	public void setZ(double z) {
		this.z = z;
	}
	
	
	/**
	 * 
	 * @param loc
	 */
	public void setLoc( Location loc ){
		if(loc != null){
			this.world_name = loc.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public World getWorld(){
		return plugin.getServer().getWorld(getWorldName());
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Location getLoc(){
		return new Location(getWorld(), getX(), getY(), getZ());
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setBlockId(int)
	 */
	public void setBlockId( int id ){
		// Water/Lava placement always turns into stationary blocks, and a rollback would
		// fail because we wouldn't detect the same block placed on rollback. So,
		// we just force record the block as stationary.
		// https://snowy-evening.com/botsko/prism/297/
		if( this.type.getName().equals("block-place") && (id == 8 || id == 10) ){
			id = (id == 8 ? 9 : 11);
		}
		this.block_id = id;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setBlockSubId(byte)
	 */
	public void setBlockSubId( int id ){
		this.block_subid = id;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getBlockId()
	 */
	public int getBlockId(){
		return block_id;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getBlockSubId()
	 */
	public int getBlockSubId(){
		return block_subid;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setOldBlockId(int)
	 */
	public void setOldBlockId( int id ){
		this.old_block_id = id;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setOldBlockSubId(byte)
	 */
	public void setOldBlockSubId( int id ){
		this.old_block_subid = id;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getOldBlockId()
	 */
	public int getOldBlockId(){
		return old_block_id;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getOldBlockSubId()
	 */
	public int getOldBlockSubId(){
		return old_block_subid;
	}

	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getData()
	 */
	public String getData() {
		return data;
	}

	// MCPC+ start
	/* (non-Javadoc)
	* @see me.botsko.prism.actions.Handler#getData()
	*/
	public String getTileEntityData() {
		return te_data;
	}

	/* (non-Javadoc)
	* @see me.botsko.prism.actions.Handler#setData(java.lang.String)
	*/
	public void setTileEntityData(String data) {
		this.te_data = data;
	}
	// MCPC+ end

	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setData(java.lang.String)
	 */
	public void setData(String data) {
		this.data = data;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setMaterialAliases(me.botsko.prism.MaterialAliases)
	 */
	public void setMaterialAliases( MaterialAliases m ){
		this.materialAliases = m;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#setAggregateCount(int)
	 */
	public void setAggregateCount( int aggregateCount ){
		this.aggregateCount = aggregateCount;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getAggregateCount()
	 */
	public int getAggregateCount(){
		return aggregateCount;
	}
	
	
	/* (non-Javadoc)
	 * @see me.botsko.prism.actions.Handler#getNiceName()
	 */
	public String getNiceName(){
		return "something";
	}
	
	
	/**
	 * 
	 */
	public boolean isCanceled(){
		return canceled;
	}
	
	
	/**
	 * 
	 */
	public void setCanceled( boolean cancel ){
		this.canceled = cancel;
	}
	
	
	/**
	 * 
	 */
	public void save(){
		// data is already set - anything not encoding a json
		// object is already ready.
	}
	
	
	/**
	 * 
	 */
	public ChangeResult applyRollback( Player player, QueryParameters parameters, boolean is_preview ){
		return null;
	}
	
	
	/**
	 * 
	 */
	public ChangeResult applyRestore( Player player, QueryParameters parameters, boolean is_preview ){
		return null;
	}
	
	
	/**
	 * 
	 */
	public ChangeResult applyUndo( Player player, QueryParameters parameters, boolean is_preview ){
		return null;
	}
	
	
	/**
	 * 
	 */
	public ChangeResult applyDeferred( Player player, QueryParameters parameters, boolean is_preview ){
		return null;
	}
}