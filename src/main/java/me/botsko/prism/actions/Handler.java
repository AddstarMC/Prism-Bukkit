package me.botsko.prism.actions;

import java.util.UUID;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.data.BlockData;
import org.bukkit.entity.Player;

import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;

public interface Handler {
	/**
	 * @return the id
	 */
	public abstract long getId();

	/**
	 * @param id the id to set
	 */
	public abstract void setId(long id);

	/**
	 * @return the action_time
	 */
	public abstract long getUnixEpoch();

	/**
	 * @return the display_date
	 */
	public abstract String getDisplayDate();

	/**
	 * @return the display_time
	 */
	public abstract String getDisplayTime();

	/**
	 * @param display_time the display_time to set
	 */
	public abstract void setUnixEpoch(long epoch);

	/**
	 * 
	 * @return
	 */
	public abstract String getTimeSince();

	/**
	 * @return the action_type
	 */
	public abstract ActionType getActionType();

	/**
	 * 
	 * @param type
	 */
	public abstract void setActionType(ActionType type);

	/**
	 * @param world the world to set
	 */
	public abstract void setWorld(World world);
	
	public abstract Location getLoc();

	/**
	 * @return the name of the event cause
	 */
	public abstract String getSourceName();

	/**
	 * @param name the custom name for the event cause
	 */
	public abstract void setSourceName(String name);


	/**
	 * @param x the x to set
	 */
	public abstract void setX(double x);

	/**
	 * @param y the y to set
	 */
	public abstract void setY(double y);

	/**
	 * @param z the z to set
	 */
	public abstract void setZ(double z);

	/**
	 * 
	 * @param id
	 */
	public abstract void setMaterial(Material material);

	/**
	 * 
	 * @param id
	 */
	public abstract void setBlockData(BlockData state);

	public abstract void setDurability(short durability);

	/**
	 * 
	 */
	public abstract Material getMaterial();

	/**
	 * 
	 */
	public abstract BlockData getBlockData();

	public abstract short getDurability();
	
	public abstract String serialize();
	
	public abstract void deserialize(String data);

	/**
	 * 
	 * @param id
	 */
	public abstract void setOldMaterial(Material material);

	/**
	 * 
	 * @param id
	 */
	public abstract void setOldBlockData(BlockData state);

	public abstract void setOldDurability(short durability);

	/**
	 * 
	 */
	public abstract Material getOldMaterial();

	/**
	 * 
	 */
	public abstract BlockData getOldBlockData();

	public abstract short getOldDurability();

	/**
	 * 
	 * @param aggregateCount
	 */
	public abstract void setAggregateCount(int aggregateCount);

	/**
	 * 
	 * @return
	 */
	public abstract int getAggregateCount();

	/**
	 * 
	 */
	public abstract String getNiceName();
	
	void setUUID(UUID uuid);
	UUID getUUID();

	/**
	 *
	 */
	public abstract boolean isCanceled();

	/**
	 * 
	 * @param cancel
	 */
	public abstract void setCanceled(boolean cancel);

	/**
	 * 
	 * @param player
	 * @param parameters
	 * @param is_preview
	 * @return
	 */
	public abstract ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview);

	/**
	 * 
	 * @param player
	 * @param parameters
	 * @param is_preview
	 * @return
	 */
	public abstract ChangeResult applyRestore(Player player, QueryParameters parameters, boolean is_preview);

	/**
	 * 
	 * @param player
	 * @param parameters
	 * @param is_preview
	 * @return
	 */
	public abstract ChangeResult applyUndo(Player player, QueryParameters parameters, boolean is_preview);

	/**
	 * 
	 * @param player
	 * @param parameters
	 * @param is_preview
	 * @return
	 */
	public abstract ChangeResult applyDeferred(Player player, QueryParameters parameters, boolean is_preview);

	public String getCustomDesc();

	public void setCustomDesc(String description);

}