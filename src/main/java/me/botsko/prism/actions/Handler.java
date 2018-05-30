package me.botsko.prism.actions;

import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;

import me.botsko.prism.utils.MaterialAliases;
import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;

public interface Handler {

	/**
	 * 
	 * @param pl
	 */
	public abstract void setPlugin(Plugin pl);

	/**
	 * @return the id
	 */
	public abstract long getId();

	/**
	 * @param id
	 *            the id to set
	 */
	public abstract void setId(long id);

	/**
	 * @return the action_time
	 */
	public abstract String getUnixEpoch();

	/**
	 * @return the display_date
	 */
	public abstract String getDisplayDate();

	/**
	 * @return the display_time
	 */
	public abstract String getDisplayTime();

	/**
	 * @param display_time
	 *            the display_time to set
	 */
	public abstract void setUnixEpoch(String epoch);

	/**
	 * 
	 * @return
	 */
	public abstract String getTimeSince();

	/**
	 * @return the action_type
	 */
	public abstract ActionType getType();

	/**
	 * 
	 * @param type
	 */
	public abstract void setType(ActionType type);

	/**
	 * @return the world_name
	 */
	public abstract String getWorldName();

	/**
	 * @param world_name
	 *            the world_name to set
	 */
	public abstract void setWorldName(String world_name);

	/**
	 * @return the player_name
	 */
	public abstract String getPlayerName();

	/**
	 * @param player_name
	 *            the player_name to set
	 */
	public abstract void setNonPlayerName(String player_name);

	/**
	 * @return the x
	 */
	public abstract double getX();

	/**
	 * @param x
	 *            the x to set
	 */
	public abstract void setX(double x);

	/**
	 * @return the y
	 */
	public abstract double getY();

	/**
	 * @param y
	 *            the y to set
	 */
	public abstract void setY(double y);

	/**
	 * @return the z
	 */
	public abstract double getZ();

	/**
	 * @param z
	 *            the z to set
	 */
	public abstract void setZ(double z);

	/**
	 * 
	 * @param id
	 */
	public abstract void setBlock(Material material);

	/**
	 * 
	 * @param id
	 */
	// TODO: Safe until the flattening in 1.13
	public abstract void setBlockSubId(int id);

	/**
	 * 
	 */
	public abstract Material getBlock();

	/**
	 * 
	 */
	// TODO: Safe until the flattening in 1.13
	public abstract int getBlockSubId();

	/**
	 * 
	 * @param id
	 */
	public abstract void setOldBlock(Material material);

	/**
	 * 
	 * @param id
	 */
	// TODO: Safe until the flattening in 1.13
	public abstract void setOldBlockSubId(int id);

	/**
	 * 
	 */
	public abstract Material getOldBlock();

	/**
	 * 
	 */
	// TODO: Safe until the flattening in 1.13
	public abstract int getOldBlockSubId();

	/**
	 * @return the data
	 */
	public abstract String getData();

	/**
	 * @param data
	 *            the data to set
	 */
	public abstract void setData(String data);

	/**
	 * 
	 * @param m
	 */
	public abstract void setMaterialAliases(MaterialAliases m);

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

	/**
	 * 
	 */
	public abstract void save();

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

}