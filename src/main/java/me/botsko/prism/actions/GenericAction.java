package me.botsko.prism.actions;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.data.BlockData;
import org.bukkit.entity.AnimalTamer;
import org.bukkit.entity.Player;

import java.text.SimpleDateFormat;
import java.util.UUID;

public abstract class GenericAction implements Handler {
    private static final SimpleDateFormat date = new SimpleDateFormat("yy/MM/dd");
    private static final SimpleDateFormat time = new SimpleDateFormat("hh:mm:ssa");
    private final Gson gson = new GsonBuilder().disableHtmlEscaping().create();
    private boolean canceled = false;
    private ActionType type;

    private long id;

    private long epoch;

    private String sourceName;

    private UUID playerUuid;

    private Location location;

    private Material material = Material.AIR;

    private BlockData blockData;

    private Material oldBlock = Material.AIR;

    private BlockData oldBlockData;

    private int aggregateCount = 0;

    public GenericAction() {
        epoch = System.currentTimeMillis() / 1000;
        ActionMeter.mark(this.getClass());
    }

    protected final Gson gson() {
        return gson;
    }

    @Override
    public String getCustomDesc() {
        return null;
    }

    @Override
    public void setCustomDesc(String description) {
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getId()
     */
    @Override
    public long getId() {
        return id;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setId(int)
     */
    @Override
    public void setId(long id) {
        this.id = id;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getActionTime()
     */
    @Override
    public long getUnixEpoch() {
        return epoch;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setDisplayDate(java.lang.String)
     */
    @Override
    public void setUnixEpoch(long epoch) {
        this.epoch = epoch;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getDisplayDate()
     */
    @Override
    public String getDisplayDate() {
        return date.format(epoch * 1000);
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getDisplayTime()
     */
    @Override
    public String getDisplayTime() {
        return time.format(epoch * 1000);
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getTimeSince()
     */
    @Override
    public String getTimeSince() {
        long diffInSeconds = System.currentTimeMillis() / 1000 - epoch;

        if (diffInSeconds < 60) {
            return "just now";
        }

        long period = 24 * 60 * 60;

        final long[] diff = {
                diffInSeconds / period,
                (diffInSeconds / (period /= 24)) % 24,
                (diffInSeconds / (period /= 60)) % 60
        };

        StringBuilder timeAgo = new StringBuilder();

        if (diff[0] > 0) {
            timeAgo.append(diff[0]).append('d');
        }

        if (diff[1] > 0) {
            timeAgo.append(diff[1]).append('h');
        }

        if (diff[2] > 0) {
            timeAgo.append(diff[2]).append('m');
        }

        // 'time_ago' will have something at this point, because if all 'diff's
        // were 0, the first if check would have caught and returned "just now"
        return timeAgo.append(" ago").toString();

    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getType()
     */
    @Override
    public ActionType getActionType() {
        return type;
    }

    /**
     * Set Action Type from a String.
     *
     * @param actionType String
     */
    public void setActionType(String actionType) {
        if (actionType != null) {
            setActionType(Prism.getActionRegistry().getAction(actionType));
        }
    }

    /**
     * Set the Action Type.
     *
     * @param type {@link ActionType}
     */
    @Override
    public void setActionType(ActionType type) {
        this.type = type;
    }

    private void createWorldIfNull() {
        if (location == null) {
            location = Bukkit.getWorlds().get(0).getSpawnLocation();
        }
    }

    /**
     * Set the player.
     *
     * @param player OfflinePlayer
     */
    public void setPlayer(AnimalTamer player) {
        if (player != null) {
            setUuid(player.getUniqueId());
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getPlayerName()
     */
    @Override
    public String getSourceName() {
        if (sourceName != null) {
            return sourceName;
        }

        return Bukkit.getOfflinePlayer(playerUuid).getName();
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setPlayerName(java.lang.String)
     */
    @Override
    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
        this.playerUuid = null;
    }

    @Override
    public UUID getUuid() {
        return playerUuid;
    }

    @Override
    public void setUuid(UUID uuid) {
        this.playerUuid = uuid;
        this.sourceName = null;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setX(double)
     */
    @Override
    public void setX(double x) {
        createWorldIfNull();
        location.setX(x);
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setY(double)
     */
    @Override
    public void setY(double y) {
        createWorldIfNull();
        location.setY(y);
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setZ(double)
     */
    @Override
    public void setZ(double z) {
        createWorldIfNull();
        location.setZ(z);
    }

    /**
     * Get World.
     *
     * @return World
     */
    public World getWorld() {
        if (location != null) {
            return location.getWorld();
        }

        return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setWorld(org.bukkit.World)
     */
    @Override
    public void setWorld(World world) {
        createWorldIfNull();
        location.setWorld(world);
    }

    /**
     * Get Location.
     *
     * @return Location
     */
    public Location getLoc() {
        return location;
    }

    /**
     * Set Location.
     *
     * @param loc Location
     */
    public void setLoc(Location loc) {
        if (loc != null) {
            location = loc.clone();
        } else {
            location = null;
        }
    }

    @Override
    public Material getMaterial() {
        return material;
    }

    @Override
    public void setMaterial(Material material) {
        this.material = material;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getBlockSubId()
     */
    @Override
    public BlockData getBlockData() {
        return blockData;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setBlockSubId(byte)
     */
    @Override
    public void setBlockData(BlockData data) {
        this.blockData = data;
    }

    @Override
    public short getDurability() {
        return 0;
    }

    @Override
    public void setDurability(short durability) {
    }

    @Override
    public Material getOldMaterial() {
        return oldBlock;
    }

    @Override
    public void setOldMaterial(Material material) {
        this.oldBlock = material;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getOldBlockSubId()
     */
    @Override
    public BlockData getOldBlockData() {
        return oldBlockData;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setOldBlockSubId(byte)
     */
    @Override
    public void setOldBlockData(BlockData data) {
        this.oldBlockData = data;
    }

    @Override
    public short getOldDurability() {
        return 0;
    }

    @Override
    public void setOldDurability(short durability) {
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#getAggregateCount()
     */
    @Override
    public int getAggregateCount() {
        return aggregateCount;
    }

    /*
     * (non-Javadoc)
     *
     * @see me.botsko.prism.actions.Handler#setAggregateCount(int)
     */
    @Override
    public void setAggregateCount(int aggregateCount) {
        this.aggregateCount = aggregateCount;
    }

    @Override
    public boolean isCanceled() {
        return canceled;
    }


    @Override
    public void setCanceled(boolean cancel) {
        this.canceled = cancel;
    }


    @Override
    public ChangeResult applyRollback(Player player, QueryParameters params, boolean isPreview) {
        return null;
    }

    @Override
    public ChangeResult applyRestore(Player player, QueryParameters parameters, boolean isPreview) {
        return null;
    }

    @Override
    public ChangeResult applyUndo(Player player, QueryParameters parameters, boolean isPreview) {
        return null;
    }

    @Override
    public ChangeResult applyDeferred(Player player, QueryParameters params, boolean isPreview) {
        return null;
    }
}
