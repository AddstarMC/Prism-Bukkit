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
	long getId();

	/**
	 * @param id the id to set
	 */
	void setId(long id);

	/**
	 * @return the action_time
	 */
	long getUnixEpoch();

	/**
	 * @return the display_date
	 */
	String getDisplayDate();

	/**
	 * @return the display_time
	 */
	String getDisplayTime();

	/**
	 * @param epoch the display_time to set
	 */
	void setUnixEpoch(long epoch);

	boolean hasExtraData();
	/**
	 * 
	 * @return
	 */
	String getTimeSince();

	/**
	 * @return the action_type
	 */
	ActionType getActionType();

	/**
	 * 
	 * @param type
	 */
	void setActionType(ActionType type);

	/**
	 * @param world the world to set
	 */
	void setWorld(World world);

	Location getLoc();

	/**
	 * @return the name of the event cause
	 */
	String getSourceName();

	/**
	 * @param name the custom name for the event cause
	 */
	void setSourceName(String name);


	/**
	 * @param x the x to set
	 */
	void setX(double x);

	/**
	 * @param y the y to set
	 */
	void setY(double y);

	/**
	 * @param z the z to set
	 */
	void setZ(double z);

	/**
	 * 
	 * @param material
	 */
	void setMaterial(Material material);

	/**
	 * 
	 * @param state
	 */
	void setBlockData(BlockData state);

	void setDurability(short durability);

	/**
	 * 
	 */
	Material getMaterial();

	/**
	 * 
	 */
	BlockData getBlockData();

	short getDurability();

	String serialize();

	void deserialize(String data);

	/**
	 * 
	 * @param material
	 */
	void setOldMaterial(Material material);

	/**
	 * 
	 * @param state
	 */
	void setOldBlockData(BlockData state);

	void setOldDurability(short durability);

	/**
	 * 
	 */
	Material getOldMaterial();

	/**
	 * 
	 */
	BlockData getOldBlockData();

	short getOldDurability();

	/**
	 * 
	 * @param aggregateCount
	 */
	void setAggregateCount(int aggregateCount);

	/**
	 * 
	 * @return
	 */
	int getAggregateCount();

	/**
	 * 
	 */
	String getNiceName();
	
	void setUUID(UUID uuid);
	UUID getUUID();

	/**
	 *
	 */
	boolean isCanceled();

	/**
	 * 
	 * @param cancel
	 */
	void setCanceled(boolean cancel);


	void setWasRollback(int rollback);

	int getWasRollback();

	/**
	 * 
	 * @param player
	 * @param parameters
	 * @param is_preview
	 * @return
	 */
	ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview);

	/**
	 * 
	 * @param player
	 * @param parameters
	 * @param is_preview
	 * @return
	 */
	ChangeResult applyRestore(Player player, QueryParameters parameters, boolean is_preview);

	/**
	 * 
	 * @param player
	 * @param parameters
	 * @param is_preview
	 * @return
	 */
	ChangeResult applyUndo(Player player, QueryParameters parameters, boolean is_preview);

	/**
	 * 
	 * @param player
	 * @param parameters
	 * @param is_preview
	 * @return
	 */
	ChangeResult applyDeferred(Player player, QueryParameters parameters, boolean is_preview);

	String getCustomDesc();

	void setCustomDesc(String description);

}
