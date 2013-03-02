package me.botsko.prism.actions;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import me.botsko.prism.MaterialAliases;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionType;

import org.bukkit.craftbukkit.libs.com.google.gson.Gson;
import org.bukkit.craftbukkit.libs.com.google.gson.GsonBuilder;


public class GenericAction implements Action {
	
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
	protected String action_time;
	
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
	protected byte block_subid;
	
	/**
	 * 
	 */
	protected int old_block_id;
	
	/**
	 * 
	 */
	protected byte old_block_subid;
	
	/**
	 * 
	 */
	protected String data;
	
	/**
	 * 
	 */
	protected int aggregateCount = 0;
	
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public GenericAction( String action_type, String player ){
		
		// Store information for the action
		if(action_type != null){
			this.type = Prism.getActionRegistry().getAction( action_type );
		}
		if(player != null){
			this.player_name = player;
		}
	}


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
	 * @return the action_time
	 */
	public String getActionTime() {
		return action_time;
	}

	
	/**
	 * @param action_time the action_time to set
	 */
	public void setActionTime(String action_time) {
		this.action_time = action_time;
	}

	
	/**
	 * @return the display_date
	 */
	public String getDisplayDate() {
		return display_date;
	}

	
	/**
	 * @param display_date the display_date to set
	 */
	public void setDisplayDate(String display_date) {
		this.display_date = display_date;
	}

	
	/**
	 * @return the display_time
	 */
	public String getDisplayTime() {
		return display_time;
	}

	
	/**
	 * @param display_time the display_time to set
	 */
	public void setDisplayTime(String display_time) {
		this.display_time = display_time;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getTimeSince(){
		
		String time_ago = "";
		
		try {
			
			Date start = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(this.action_time);
			Date end = new Date();
			
			long diffInSeconds = (end.getTime() - start.getTime()) / 1000;

		    long diff[] = new long[] { 0, 0, 0, 0 };
		    /* sec */	diff[3] = (diffInSeconds >= 60 ? diffInSeconds % 60 : diffInSeconds);
		    /* min */	diff[2] = (diffInSeconds = (diffInSeconds / 60)) >= 60 ? diffInSeconds % 60 : diffInSeconds;
		    /* hours */	diff[1] = (diffInSeconds = (diffInSeconds / 60)) >= 24 ? diffInSeconds % 24 : diffInSeconds;
		    /* days */	diff[0] = (diffInSeconds = (diffInSeconds / 24));

		    // Only show days if more than 1
		    if(diff[0] > 1){
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
			
		} catch (ParseException e) {
			e.printStackTrace();
			return "";
		}
		return time_ago;
	}

	
	/**
	 * @return the action_type
	 */
	public ActionType getType() {
		return type;
	}
	
	
	/**
	 * 
	 * @param type
	 */
	public void setType( ActionType type ){
		this.type = type;
	}

	
	/**
	 * @return the world_name
	 */
	public String getWorldName() {
		return world_name;
	}

	
	/**
	 * @param world_name the world_name to set
	 */
	public void setWorldName(String world_name) {
		this.world_name = world_name;
	}

	
	/**
	 * @return the player_name
	 */
	public String getPlayerName() {
		return player_name;
	}

	
	/**
	 * @param player_name the player_name to set
	 */
	public void setPlayerName(String player_name) {
		this.player_name = player_name;
	}

	
	/**
	 * @return the x
	 */
	public double getX() {
		return x;
	}

	
	/**
	 * @param x the x to set
	 */
	public void setX(double x) {
		this.x = x;
	}

	
	/**
	 * @return the y
	 */
	public double getY() {
		return y;
	}

	
	/**
	 * @param y the y to set
	 */
	public void setY(double y) {
		this.y = y;
	}

	
	/**
	 * @return the z
	 */
	public double getZ() {
		return z;
	}

	
	/**
	 * @param z the z to set
	 */
	public void setZ(double z) {
		this.z = z;
	}
	
	
	/**
	 * 
	 * @param id
	 */
	public void setBlockId( int id ){
		this.block_id = id;
	}
	
	
	/**
	 * 
	 * @param id
	 */
	public void setBlockSubId( byte id ){
		this.block_subid = id;
	}
	
	
	/**
	 * 
	 */
	public int getBlockId(){
		return block_id;
	}
	
	
	/**
	 * 
	 */
	public byte getBlockSubId(){
		return block_subid;
	}
	
	
	/**
	 * 
	 * @param id
	 */
	public void setOldBlockId( int id ){
		this.old_block_id = id;
	}
	
	
	/**
	 * 
	 * @param id
	 */
	public void setOldBlockSubId( byte id ){
		this.old_block_subid = id;
	}
	
	
	/**
	 * 
	 */
	public int getOldBlockId(){
		return old_block_id;
	}
	
	
	/**
	 * 
	 */
	public byte getOldBlockSubId(){
		return old_block_subid;
	}

	
	/**
	 * @return the data
	 */
	public String getData() {
		return data;
	}

	
	/**
	 * @param data the data to set
	 */
	public void setData(String data) {
		this.data = data;
	}
	
	
	/**
	 * 
	 * @param m
	 */
	public void setMaterialAliases( MaterialAliases m ){
		this.materialAliases = m;
	}
	
	
	/**
	 * 
	 * @param aggregateCount
	 */
	public void setAggregateCount( int aggregateCount ){
		this.aggregateCount = aggregateCount;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public int getAggregateCount(){
		return aggregateCount;
	}
	
	
	/**
	 * 
	 */
	public String getNiceName(){
		return "something";
	}
}