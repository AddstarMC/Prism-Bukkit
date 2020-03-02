package me.botsko.prism.actions;

import java.text.SimpleDateFormat;
import java.util.UUID;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.World;
import org.bukkit.block.data.BlockData;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.bukkit.entity.Player;

public abstract class GenericAction implements Handler {
	private static final SimpleDateFormat date = new SimpleDateFormat("yy/MM/dd");
	private static final SimpleDateFormat time = new SimpleDateFormat("hh:mm:ssa");

	/**
	 * 
	 */
	private boolean canceled = false;

	/**
	 * 
	 */
	private final Gson gson = new GsonBuilder().disableHtmlEscaping().create();

	/**
	 * 
	 */
	private ActionType type;

	/**
	 * 
	 */
	private long id;

	/**
	 * 
	 */
	private long epoch;
	
	/**
	 * 
	 */
	private String sourceName;
	private UUID playerUUID;
	
	private Location location;

	/**
	 * 
	 */
	private Material material = Material.AIR;

	/**
	 * 
	 */
	private BlockData blockData;

	/**
	 * 
	 */
	private Material oldBlock = Material.AIR;

	/**
	 * 
	 */
	private BlockData oldBlockData;

	/**
	 * 
	 */
	private int aggregateCount = 0;

	private int rollback;
	
	public GenericAction() {
		epoch = System.currentTimeMillis() / 1000;
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

	/**
	 * 
	 * @param action_type
	 */
	public void setActionType(String action_type) {
		if (action_type != null) {
			setActionType(Prism.getActionRegistry().getAction(action_type));
		}
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
	 * @see me.botsko.prism.actions.Handler#getDisplayDate()
	 */
	@Override
	public String getDisplayDate() {
		return date.format(epoch * 1000);
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
		
		if(diffInSeconds < 60) {
			return "just now";
		}
		
		long period = 24 * 60 * 60;

		final long[] diff = {
				diffInSeconds / period, // days
				(diffInSeconds / (period /= 24)) % 24, // hours
				(diffInSeconds / (period /= 60)) % 60, // minutes
		};
		
		StringBuilder time_ago = new StringBuilder();

		if (diff[0] > 0) {
			time_ago.append(diff[0]).append('d');
		}
		
		if (diff[1] > 0) {
			time_ago.append(diff[1]).append('h');
		}
		
		if (diff[2] > 0) {
			time_ago.append(diff[2]).append('m');
		}
		
		// 'time_ago' will have something at this point, because if all 'diff's
		// were 0, the first if check would have caught and returned "just now"
		return time_ago.append(" ago").toString();

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

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * me.botsko.prism.actions.Handler#setType(me.botsko.prism.actionlibs.ActionType
	 * )
	 */
	@Override
	public void setActionType(ActionType type) {
		this.type = type;
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
	
	private void createWorldIfNull() {
		if(location == null) {
			location = Bukkit.getWorlds().get(0).getSpawnLocation();
		}
	}

	/**
	 * 
	 * @param player
	 */
	public void setPlayer(OfflinePlayer player) {
		if (player != null) {
			setUUID(player.getUniqueId());
		}
	}

	@Override
	public void setUUID(UUID uuid) {
		this.playerUUID = uuid;
		this.sourceName = null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see me.botsko.prism.actions.Handler#getPlayerName()
	 */
	@Override
	public String getSourceName() {
		if(sourceName != null) {
			return sourceName;
		}
		
		return Bukkit.getOfflinePlayer(playerUUID).getName();
	}

	@Override
	public UUID getUUID() {
		return playerUUID;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see me.botsko.prism.actions.Handler#setPlayerName(java.lang.String)
	 */
	@Override
	public void setSourceName(String source_name) {
		this.sourceName = source_name;
		this.playerUUID = null;
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
	 * 
	 * @param loc
	 */
	public void setLoc(Location loc) {
		if (loc != null) {
			location = loc.clone();
		}
		else {
			location = null;
		}
	}

	/**
	 * 
	 * @return
	 */
	public World getWorld() {
		if(location != null) {
			return location.getWorld();
		}
		
		return null;
	}

	/**
	 * 
	 * @return
	 */
	public Location getLoc() {
		return location;
	}

	@Override
	public void setMaterial(Material material) {
		this.material = material;
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
	public void setDurability(short durability) {
	}

	@Override
	public Material getMaterial() {
		return material;
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

	@Override
	public short getDurability() {
		return 0;
	}

	@Override
	public void setOldMaterial(Material material) {
		this.oldBlock = material;
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
	public void setOldDurability(short durability) {
	}

	@Override
	public Material getOldMaterial() {
		return oldBlock;
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

	@Override
	public short getOldDurability() {
		return 0;
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see me.botsko.prism.actions.Handler#getAggregateCount()
	 */
	@Override
	public int getAggregateCount() {
		return aggregateCount;
	}

	/**
	 * 
	 */
	@Override
	public boolean isCanceled() {
		return canceled;
	}

	/**
	 * 
	 */
	@Override
	public void setCanceled(boolean cancel) {
		this.canceled = cancel;
	}

	@Override
	public void setWasRollback(int rollback) {
		this.rollback = rollback;
	}

	@Override
	public int getWasRollback() {
		return rollback;
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview) {
		return null;
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyRestore(Player player, QueryParameters parameters, boolean is_preview) {
		return null;
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyUndo(Player player, QueryParameters parameters, boolean is_preview) {
		return null;
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyDeferred(Player player, QueryParameters parameters, boolean is_preview) {
		return null;
	}
}
