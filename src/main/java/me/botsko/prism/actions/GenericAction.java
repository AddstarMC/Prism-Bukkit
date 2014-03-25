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
    protected final Gson gson = new GsonBuilder().disableHtmlEscaping().create();

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

    /**
	 * 
	 */
    protected int aggregateCount = 0;

    /**
	 * 
	 */
    @Override
    public void setPlugin(Plugin plugin) {
        this.plugin = plugin;
    }

    /**
     * 
     * @param action_type
     */
    public void setActionType(String action_type) {
        if( action_type != null ) {
            this.type = Prism.getActionRegistry().getAction( action_type );
        }
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getId()
     */
    @Override
    public int getId() {
        return id;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setId(int)
     */
    @Override
    public void setId(int id) {
        this.id = id;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getActionTime()
     */
    @Override
    public String getUnixEpoch() {
        return epoch;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getDisplayDate()
     */
    @Override
    public String getDisplayDate() {
        return display_date;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setDisplayDate(java.lang.String)
     */
    @Override
    public void setUnixEpoch(String epoch) {

        this.epoch = epoch;

        final Date action_time = new Date( Long.parseLong( epoch ) * 1000 );

        final SimpleDateFormat date = new SimpleDateFormat( "yy/MM/dd" );
        this.display_date = date.format( action_time );

        final SimpleDateFormat time = new SimpleDateFormat( "h:m:sa" );
        this.display_time = time.format( action_time );

    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getDisplayTime()
     */
    @Override
    public String getDisplayTime() {
        return display_time;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getTimeSince()
     */
    @Override
    public String getTimeSince() {

        String time_ago = "";

        final Date start = new Date( Long.parseLong( this.epoch ) * 1000 );
        final Date end = new Date();

        long diffInSeconds = ( end.getTime() - start.getTime() ) / 1000;

        final long diff[] = new long[] { 0, 0, 0, 0 };
        /* sec */diff[3] = ( diffInSeconds >= 60 ? diffInSeconds % 60 : diffInSeconds );
        /* min */diff[2] = ( diffInSeconds = ( diffInSeconds / 60 ) ) >= 60 ? diffInSeconds % 60 : diffInSeconds;
        /* hours */diff[1] = ( diffInSeconds = ( diffInSeconds / 60 ) ) >= 24 ? diffInSeconds % 24 : diffInSeconds;
        /* days */diff[0] = ( diffInSeconds = ( diffInSeconds / 24 ) );

        // Only show days if more than 1
        if( diff[0] >= 1 ) {
            time_ago += diff[0] + "d";
        }
        // Only show hours if > 1
        if( diff[1] >= 1 ) {
            time_ago += diff[1] + "h";
        }
        // Only show minutes if > 1 and less than 60
        if( diff[2] > 1 && diff[2] < 60 ) {
            time_ago += diff[2] + "m";
        }
        if( !time_ago.isEmpty() ) {
            time_ago += " ago";
        }

        if( diff[0] == 0 && diff[1] == 0 && diff[2] <= 1 ) {
            time_ago = "just now";
        }

        return time_ago;

    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getType()
     */
    @Override
    public ActionType getType() {
        return type;
    }

    /*
     * (non-Javadoc)
     * @see
     * me.botsko.prism.actions.Handler#setType(me.botsko.prism.actionlibs.ActionType
     * )
     */
    @Override
    public void setType(ActionType type) {
        this.type = type;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getWorldName()
     */
    @Override
    public String getWorldName() {
        return world_name;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setWorldName(java.lang.String)
     */
    @Override
    public void setWorldName(String world_name) {
        this.world_name = world_name;
    }

    /**
     * 
     * @param player
     */
    public void setPlayerName(Player player) {
        if( player != null ) {
            this.player_name = player.getName();
        }
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getPlayerName()
     */
    @Override
    public String getPlayerName() {
        return player_name;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setPlayerName(java.lang.String)
     */
    @Override
    public void setPlayerName(String player_name) {
        this.player_name = player_name;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getX()
     */
    @Override
    public double getX() {
        return x;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setX(double)
     */
    @Override
    public void setX(double x) {
        this.x = x;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getY()
     */
    @Override
    public double getY() {
        return y;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setY(double)
     */
    @Override
    public void setY(double y) {
        this.y = y;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getZ()
     */
    @Override
    public double getZ() {
        return z;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setZ(double)
     */
    @Override
    public void setZ(double z) {
        this.z = z;
    }

    /**
     * 
     * @param loc
     */
    public void setLoc(Location loc) {
        if( loc != null ) {
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
    public World getWorld() {
        return plugin.getServer().getWorld( getWorldName() );
    }

    /**
     * 
     * @return
     */
    public Location getLoc() {
        return new Location( getWorld(), getX(), getY(), getZ() );
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setBlockId(int)
     */
    @Override
    public void setBlockId(int id) {
        // Water/Lava placement always turns into stationary blocks, and a
        // rollback would
        // fail because we wouldn't detect the same block placed on rollback.
        // So,
        // we just force record the block as stationary.
        // https://snowy-evening.com/botsko/prism/297/
        if( this.type.getName().equals( "block-place" ) && ( id == 8 || id == 10 ) ) {
            id = ( id == 8 ? 9 : 11 );
        }
        this.block_id = id;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setBlockSubId(byte)
     */
    @Override
    public void setBlockSubId(int id) {
        this.block_subid = id;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getBlockId()
     */
    @Override
    public int getBlockId() {
        return block_id;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getBlockSubId()
     */
    @Override
    public int getBlockSubId() {
        return block_subid;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setOldBlockId(int)
     */
    @Override
    public void setOldBlockId(int id) {
        this.old_block_id = id;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setOldBlockSubId(byte)
     */
    @Override
    public void setOldBlockSubId(int id) {
        this.old_block_subid = id;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getOldBlockId()
     */
    @Override
    public int getOldBlockId() {
        return old_block_id;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getOldBlockSubId()
     */
    @Override
    public int getOldBlockSubId() {
        return old_block_subid;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getData()
     */
    @Override
    public String getData() {
        return data;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setData(java.lang.String)
     */
    @Override
    public void setData(String data) {
        this.data = data;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setMaterialAliases(me.botsko.prism.
     * MaterialAliases)
     */
    @Override
    public void setMaterialAliases(MaterialAliases m) {
        this.materialAliases = m;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#setAggregateCount(int)
     */
    @Override
    public void setAggregateCount(int aggregateCount) {
        this.aggregateCount = aggregateCount;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getAggregateCount()
     */
    @Override
    public int getAggregateCount() {
        return aggregateCount;
    }

    /*
     * (non-Javadoc)
     * @see me.botsko.prism.actions.Handler#getNiceName()
     */
    @Override
    public String getNiceName() {
        return "something";
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

    /**
	 * 
	 */
    @Override
    public void save() {
        // data is already set - anything not encoding a json
        // object is already ready.
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