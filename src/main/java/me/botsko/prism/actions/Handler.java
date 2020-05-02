package me.botsko.prism.actions;

import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.data.BlockData;
import org.bukkit.entity.Player;

import java.util.UUID;

public interface Handler {

    long getId();

    void setId(long id);

    long getUnixEpoch();

    void setUnixEpoch(long epoch);

    String getDisplayDate();

    String getDisplayTime();

    boolean hasExtraData();

    String getTimeSince();

    ActionType getActionType();

    void setActionType(ActionType type);

    void setWorld(World world);

    Location getLoc();

    String getSourceName();

    void setSourceName(String name);

    void setX(double x);

    void setY(double y);

    void setZ(double z);

    Material getMaterial();

    void setMaterial(Material material);

    BlockData getBlockData();

    void setBlockData(BlockData state);

    short getDurability();

    void setDurability(short durability);

    String serialize();

    void deserialize(String data);

    Material getOldMaterial();

    void setOldMaterial(Material material);

    BlockData getOldBlockData();

    void setOldBlockData(BlockData state);

    short getOldDurability();

    void setOldDurability(short durability);

    int getAggregateCount();

    void setAggregateCount(int aggregateCount);

    String getNiceName();

    UUID getUuid();

    void setUuid(UUID uuid);

    boolean isCanceled();

    void setCanceled(boolean cancel);

    /**
     * Apply a rollback.
     *
     * @param player     Player
     * @param parameters Query Params
     * @param isPreview  bool
     * @return ChangeResult
     */
    ChangeResult applyRollback(Player player, QueryParameters parameters, boolean isPreview);

    /**
     * Apply a restore.
     *
     * @param player     Player
     * @param parameters Query Params
     * @param isPreview  bool
     * @return ChangeResult
     */
    ChangeResult applyRestore(Player player, QueryParameters parameters, boolean isPreview);

    /**
     * Apply a undo.
     *
     * @param player     Player
     * @param parameters Query Params
     * @param isPreview  bool
     * @return ChangeResult
     */
    ChangeResult applyUndo(Player player, QueryParameters parameters, boolean isPreview);

    /**
     * Apply a Deferred.
     *
     * @param player     Player
     * @param parameters Query Params
     * @param isPreview  bool
     * @return ChangeResult
     */
    ChangeResult applyDeferred(Player player, QueryParameters parameters, boolean isPreview);

    String getCustomDesc();

    void setCustomDesc(String description);

}