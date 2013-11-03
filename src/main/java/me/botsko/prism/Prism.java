package me.botsko.prism;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Logger;

import me.botsko.elixr.MaterialAliases;
import me.botsko.elixr.TypeUtils;
import me.botsko.prism.actionlibs.ActionRecorder;
import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.HandlerRegistry;
import me.botsko.prism.actionlibs.Ignore;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.PreviewSession;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.bridge.PrismBlockEditSessionFactory;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commands.PrismCommands;
import me.botsko.prism.commands.WhatCommand;
import me.botsko.prism.listeners.PrismBlockEvents;
import me.botsko.prism.listeners.PrismChannelChatEvents;
import me.botsko.prism.listeners.PrismCustomEvents;
import me.botsko.prism.listeners.PrismEntityEvents;
import me.botsko.prism.listeners.PrismInventoryEvents;
import me.botsko.prism.listeners.PrismPlayerEvents;
import me.botsko.prism.listeners.PrismVehicleEvents;
import me.botsko.prism.listeners.PrismWorldEvents;
import me.botsko.prism.listeners.self.PrismMiscEvents;
import me.botsko.prism.measurement.Metrics;
import me.botsko.prism.measurement.QueueStats;
import me.botsko.prism.measurement.TimeTaken;
import me.botsko.prism.monitors.OreMonitor;
import me.botsko.prism.monitors.UseMonitor;
import me.botsko.prism.purge.LogPurgeCallback;
import me.botsko.prism.purge.PurgeTask;
import me.botsko.prism.wands.Wand;

import org.apache.tomcat.jdbc.pool.DataSource;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.command.CommandExecutor;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.scheduler.BukkitTask;

import com.sk89q.worldedit.bukkit.WorldEditPlugin;

public class Prism extends JavaPlugin {

	/**
	 * Connection Pool
	 */
	private static DataSource pool = new DataSource();

	/**
	 * Protected/private
	 */
	private static String plugin_name;
	private String plugin_version;
	private MaterialAliases items;
	private Language language;
	private static Logger log = Logger.getLogger("Minecraft");
	private ArrayList<String> enabledPlugins = new ArrayList<String>();
	private static ActionRegistry actionRegistry;
	private static HandlerRegistry<?> handlerRegistry;
	private static Ignore ignore;
	protected static ArrayList<Integer> illegalBlocks;
	protected static ArrayList<String> illegalEntities;
	protected static HashMap<String,String> alertedOres = new HashMap<String,String>();

	/**
	 * Public
	 */
	public Prism prism;
	public static Messenger messenger;
	public static FileConfiguration config;
	public WorldEditPlugin plugin_worldEdit = null;
	public static ActionRecorder actionsRecorder;
	public ActionsQuery actionsQuery;
	public OreMonitor oreMonitor;
	public UseMonitor useMonitor;
	public ConcurrentHashMap<String, Wand> playersWithActiveTools = new ConcurrentHashMap<String, Wand>();
	public ConcurrentHashMap<String, PreviewSession> playerActivePreviews = new ConcurrentHashMap<String, PreviewSession>();
	public ConcurrentHashMap<String, ArrayList<Block>> playerActiveViews = new ConcurrentHashMap<String, ArrayList<Block>>();
	public ConcurrentHashMap<String, QueryResult> cachedQueries = new ConcurrentHashMap<String, QueryResult>();
	public ConcurrentHashMap<Location, Long> alertedBlocks = new ConcurrentHashMap<Location, Long>();
	public TimeTaken eventTimer;
	public QueueStats queueStats;
	public BukkitTask deleteTask;
	public int total_records_affected = 0;
	
	/**
	 * DB Foreign key caches
	 */
	public static HashMap<String,Integer> prismWorlds = new HashMap<String,Integer>();
	public static HashMap<String,Integer> prismPlayers = new HashMap<String,Integer>();
	public static HashMap<String,Integer> prismActions = new HashMap<String,Integer>();

	/**
	 * We store a basic index of blocks we anticipate will fall, so that when
	 * they do fall we can attribute them to the player who broke the original
	 * block.
	 * 
	 * Once the block fall is registered, it's removed from here, so data should
	 * not remain here long.
	 */
	public ConcurrentHashMap<String, String> preplannedBlockFalls = new ConcurrentHashMap<String, String>();

	/**
	 * VehicleCreateEvents do not include the player/entity that created it, so
	 * we need to track players right-clicking rails with minecart vehicles, or
	 * water for boats
	 */
	public ConcurrentHashMap<String, String> preplannedVehiclePlacement = new ConcurrentHashMap<String, String>();

	
	/**
	 * Enables the plugin and activates our player listeners
	 */
	@SuppressWarnings("rawtypes")
	@Override
	public void onEnable() {

		plugin_name = this.getDescription().getName();
		plugin_version = this.getDescription().getVersion();

		prism = this;

		log("Initializing Prism " + plugin_version + ". By Viveleroi.");

		// Load configuration, or install if new
		loadConfig();

		if (getConfig().getBoolean("prism.allow-metrics")) {
			try {
				Metrics metrics = new Metrics(this);
				metrics.start();
			} catch (IOException e) {
				log("MCStats submission failed.");
			}
		}

		// init db
		pool = initDbPool();
		Connection test_conn = dbc();
		if (pool == null || test_conn == null) {
			String[] dbDisabled = new String[3];
			dbDisabled[0] = "Prism will disable itself because it couldn't connect to a database.";
			dbDisabled[1] = "If you're using MySQL, check your config. Be sure MySQL is running.";
			dbDisabled[2] = "For help - try http://discover-prism.com/wiki/view/troubleshooting/";
			logSection(dbDisabled);
			disablePlugin();
		}
		if (test_conn != null) {
			try {
				test_conn.close();
			} catch (SQLException e) {
				handleDatabaseException(e);
			}
		}

		if (isEnabled()) {

			// Setup databases
			setupDatabase();
			
			// Cache world IDs
			cacheWorldPrimaryKeys();
			cacheActionPrimaryKeys();
			cacheOnlinePlayerPrimaryKeys();

			// Apply any updates
			Updater up = new Updater(this);
			up.apply_updates();

			eventTimer = new TimeTaken();
			queueStats = new QueueStats();
			handlerRegistry = new HandlerRegistry();
			actionRegistry = new ActionRegistry();
			ignore = new Ignore(this);

			// Plugins we use
			checkPluginDependancies();

			// Assign event listeners
			getServer().getPluginManager().registerEvents(new PrismBlockEvents(this), this);
			getServer().getPluginManager().registerEvents(new PrismEntityEvents(this), this);
			getServer().getPluginManager().registerEvents(new PrismWorldEvents(), this);
			getServer().getPluginManager().registerEvents(new PrismPlayerEvents(this), this);
			getServer().getPluginManager().registerEvents(new PrismInventoryEvents(this), this);
			getServer().getPluginManager().registerEvents(new PrismVehicleEvents(this), this);

			if (getConfig().getBoolean("prism.tracking.api.enabled")) {
				getServer().getPluginManager().registerEvents(new PrismCustomEvents(this), this);
			}

			// Assign Plugin listeners if enabled
			if (dependencyEnabled("Herochat") && getConfig().getBoolean("prism.tracking.player-chat")) {
				getServer().getPluginManager().registerEvents(new PrismChannelChatEvents(), this);
			}

			// Assign listeners to our own events
			// getServer().getPluginManager().registerEvents(new
			// PrismRollbackEvents(), this);
			getServer().getPluginManager().registerEvents(new PrismMiscEvents(), this);

			// Add commands
			getCommand("prism").setExecutor((CommandExecutor) new PrismCommands(this));
			getCommand("what").setExecutor((CommandExecutor) new WhatCommand(this));

			// Init re-used classes
			messenger = new Messenger(plugin_name);
			actionsRecorder = new ActionRecorder(this);
			actionsQuery = new ActionsQuery(this);
			oreMonitor = new OreMonitor(this);
			useMonitor = new UseMonitor(this);

			// Init async tasks
			actionRecorderTask();

			// Init scheduled events
			endExpiredQueryCaches();
			endExpiredPreviews();
			removeExpiredLocations();

			// Delete old data based on config
			discardExpiredDbRecords();

		}
	}

	
	/**
	 * 
	 * @return
	 */
	public static String getPrismName() {
		return plugin_name;
	}

	
	/**
	 * 
	 * @return
	 */
	public String getPrismVersion() {
		return this.plugin_version;
	}

	
	/**
	 * Load configuration and language files
	 */
	@SuppressWarnings("unchecked")
	public void loadConfig() {
		PrismConfig mc = new PrismConfig(this);
		config = mc.getConfig();
		
		// Cache config arrays we check constantly
		illegalBlocks = (ArrayList<Integer>) getConfig().getList("prism.appliers.never-place-block");
		illegalEntities = (ArrayList<String>) getConfig().getList("prism.appliers.never-spawn-entity");
		
		ConfigurationSection alertBlocks = getConfig().getConfigurationSection("prism.alerts.ores.blocks");
		if( alertBlocks != null){
			for( String key : alertBlocks.getKeys(false) ){
				alertedOres.put( key, alertBlocks.getString(key));
			}
		}
		
		// Load language files
		// language = new Language( mc.getLang() );
		// Load items db
		items = new MaterialAliases();
	}

	
	/**
	 * 
	 * @return
	 */
	public DataSource initDbPool() {

		DataSource pool = null;

		String dns = "jdbc:mysql://"
				+ config.getString("prism.mysql.hostname") + ":"
				+ config.getString("prism.mysql.port") + "/"
				+ config.getString("prism.mysql.database");
		pool = new DataSource();
		pool.setDriverClassName("com.mysql.jdbc.Driver");
		pool.setUrl(dns);
		pool.setUsername(config.getString("prism.mysql.username"));
		pool.setPassword(config.getString("prism.mysql.password"));
		pool.setInitialSize(config.getInt("prism.database.pool-initial-size"));
		pool.setMaxActive(config.getInt("prism.database.max-pool-connections"));
		pool.setMaxIdle(config.getInt("prism.database.max-idle-connections"));
		pool.setMaxWait(config.getInt("prism.database.max-wait"));
		pool.setRemoveAbandoned(true);
		pool.setRemoveAbandonedTimeout(60);
		pool.setTestOnBorrow(true);
		pool.setValidationQuery("/* ping */SELECT 1");
		pool.setValidationInterval(30000);

		return pool;
	}

	
	/**
	 * Attempt to rebuild the pool, useful for reloads and failed database
	 * connections being restored
	 */
	public void rebuildPool() {
		// Close pool connections when plugin disables
		if (pool != null) {
			pool.close();
		}
		pool = initDbPool();
	}

	
	/**
	 * 
	 * @return
	 */
	public static DataSource getPool() {
		return Prism.pool;
	}

	
	/**
	 * 
	 * @return
	 * @throws SQLException
	 */
	public static Connection dbc() {
		Connection con = null;
		try {
			con = pool.getConnection();
		} catch (SQLException e) {
			System.out.print("Database connection failed. " + e.getMessage());
			if (!e.getMessage().contains("Pool empty")) {
				e.printStackTrace();
			}
		}
		return con;
	}

	
	/**
	 * Attempt to reconnect to the database
	 * @return
	 * @throws SQLException 
	 */
	protected boolean attemptToRescueConnection( SQLException e ) throws SQLException{
		if( e.getMessage().contains("connection closed") ){
			rebuildPool();
			if( pool != null ){
				Connection conn = dbc();
				if( conn != null && !conn.isClosed() ){
					return true;
				}
			}
		}
		return false;
	}
	
	
	/**
	 * 
	 */
	public void handleDatabaseException(SQLException e) {
		// Attempt to rescue
		try {
			if( attemptToRescueConnection( e ) ){
				return;
			}
		} catch (SQLException e1){
		}
		log("Database connection error: " + e.getMessage());
		if (e.getMessage().contains("marked as crashed")) {
			String[] msg = new String[2];
			msg[0] = "If MySQL crashes during write it may corrupt it's indexes.";
			msg[1] = "Try running `CHECK TABLE prism_data` and then `REPAIR TABLE prism_data`.";
			logSection(msg);
		}
		e.printStackTrace();
	}
	
	
	/**
	 * 
	 */
	protected void setupDatabase() {

		try {
			final Connection conn = dbc();
			if (conn == null)
				return;
			
			// actions
			String query = "CREATE TABLE IF NOT EXISTS `prism_actions` (" +
					"`action_id` int(10) unsigned NOT NULL AUTO_INCREMENT," +
					"`action` varchar(25) NOT NULL," +
					"PRIMARY KEY (`action_id`)" +
					") ENGINE=InnoDB  DEFAULT CHARSET=latin1;";
			Statement st = conn.createStatement();
			st.executeUpdate(query);

			// data
			query = "CREATE TABLE IF NOT EXISTS `prism_data` (" +
					"`id` int(11) unsigned NOT NULL AUTO_INCREMENT," +
					"`epoch` int(11) unsigned NOT NULL," +
					"`action_id` int(11) unsigned NOT NULL," +
					"`player_id` int(11) unsigned NOT NULL," +
					"`world_id` int(11) unsigned NOT NULL," +
					"`x` int(11) NOT NULL," +
					"`y` int(11) NOT NULL," +
					"`z` int(11) NOT NULL," +
					"`block_id` mediumint(5) DEFAULT NULL," +
					"`block_subid` mediumint(5) DEFAULT NULL," +
					"`old_block_id` mediumint(5) DEFAULT NULL," +
					"`old_block_subid` mediumint(5) DEFAULT NULL," +
					"PRIMARY KEY (`id`)," +
					"KEY `x` (`x`)," +
					"KEY `block_id` (`block_id`)" +
					") ENGINE=InnoDB  DEFAULT CHARSET=latin1;";
			st.executeUpdate(query);
			
			// extra data
			query = "CREATE TABLE IF NOT EXISTS `prism_data_extra` ("
					+ "`extra_id` int(10) unsigned NOT NULL,"
					+ "`data_id` int(10) unsigned NOT NULL,"
					+ "`data` mediumtext NOT NULL,"
					+ "PRIMARY KEY (`extra_id`)"
					+ ") ENGINE=InnoDB DEFAULT CHARSET=latin1;";
			st.executeUpdate(query);
			
			// meta
			query = "CREATE TABLE IF NOT EXISTS `prism_meta` (" +
					"`id` int(10) unsigned NOT NULL AUTO_INCREMENT," +
					"`k` varchar(25) NOT NULL," +
					"`v` varchar(255) NOT NULL," +
					"PRIMARY KEY (`id`)" +
					") ENGINE=InnoDB  DEFAULT CHARSET=latin1;";
			st.executeUpdate(query);
			
			// players
			query = "CREATE TABLE IF NOT EXISTS `prism_players` (" +
					"`player_id` int(11) unsigned NOT NULL AUTO_INCREMENT," +
					"`player` varchar(16) NOT NULL," +
					"PRIMARY KEY (`player_id`)," +
					"KEY (`player`)" +
					") ENGINE=InnoDB  DEFAULT CHARSET=latin1;";
			st.executeUpdate(query);
			
			// worlds
			query = "CREATE TABLE IF NOT EXISTS `prism_worlds` (" +
					"`world_id` int(11) unsigned NOT NULL AUTO_INCREMENT," +
					"`world` varchar(255) NOT NULL," +
					"PRIMARY KEY (`world_id`)" +
					") ENGINE=InnoDB  DEFAULT CHARSET=latin1;";
			st.executeUpdate(query);
			
			// actions
			query = "INSERT INTO `prism_actions` VALUES (1, 'block-break'),(2, 'block-burn'),(3, 'block-fade'),(4, 'block-fall'),(5, 'block-form'),(6, 'block-place'),(7, 'block-shift'),(8, 'block-spread'),(9, 'block-use'),(10, 'bucket-fill'),(11, 'bonemeal-use'),(12, 'container-access'),(13, 'cake-eat'),(14, 'craft-item'),(15, 'creeper-explode'),(16, 'crop-trample'),(17, 'dragon-eat'),(18, 'enchant-item'),(19, 'enderman-pickup'),(20, 'enderman-place'),(21, 'entity-break'),(22, 'entity-dye'),(23, 'entity-explode'),(24, 'entity-follow'),(25, 'entity-form'),(26, 'entity-kill'),(27, 'entity-shear'),(28, 'entity-spawn'),(29, 'fireball'),(30, 'fire-spread'),(31, 'firework-launch'),(32, 'hangingitem-break'),(33, 'hangingitem-place'),(34, 'item-drop'),(35, 'item-insert'),(36, 'item-pickup'),(37, 'item-remove'),(38, 'lava-break'),(39, 'lava-bucket'),(40, 'lava-flow'),(41, 'lava-ignite'),(42, 'leaf-decay'),(43, 'lighter'),(44, 'lightning'),(45, 'mushroom-grow'),(46, 'player-chat'),(47, 'player-command'),(48, 'player-death'),(49, 'player-join'),(50, 'player-kill'),(51, 'player-quit'),(52, 'player-teleport'),(53, 'potion-splash'),(54, 'sheep-eat'),(55, 'sign-change'),(56, 'spawnegg-use'),(57, 'tnt-explode'),(58, 'tnt-prime'),(59, 'tree-grow'),(60, 'vehicle-break'),(61, 'vehicle-enter'),(62, 'vehicle-exit'),(63, 'vehicle-place'),(64, 'water-break'),(65, 'water-bucket'),(66, 'water-flow'),(67, 'world-edit'),(68, 'xp-pickup'),(69, 'prism-drain'),(70, 'prism-extinguish'),(71, 'prism-process'),(72, 'prism-rollback');";
			st.executeUpdate(query);
			
			// close
			st.close();
			conn.close();
			
		} catch (SQLException e) {
			log("Database connection error: " + e.getMessage());
			e.printStackTrace();
		}
	}
	
	
	/**
	 * 
	 */
	protected void cacheActionPrimaryKeys(){

		Connection conn = null;
		PreparedStatement s = null;
		ResultSet rs = null;
		try {

			conn = dbc();
    		s = conn.prepareStatement( "SELECT action_id, action FROM prism_actions" );
    		rs = s.executeQuery();

    		while(rs.next()){
    			debug("Loaded " + rs.getString(2) + ", id:" + rs.getInt(1));
    			prismActions.put( rs.getString(2), rs.getInt(1) );
    		}
    		
    		debug("Loaded " + prismActions.size() + " actions into the cache.");
    		
		} catch (SQLException e) {
        	handleDatabaseException( e );
        } finally {
        	if(rs != null) try { rs.close(); } catch (SQLException e) {}
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
	}
	
	
	/**
	 * 
	 */
	protected void cacheWorldPrimaryKeys(){
		
		Connection conn = null;
		PreparedStatement s = null;
		ResultSet rs = null;
		try {

			conn = dbc();
    		s = conn.prepareStatement( "SELECT world_id, world FROM prism_worlds" );
    		rs = s.executeQuery();

    		while(rs.next()){
    			prismWorlds.put( rs.getString(2), rs.getInt(1) );
    		}
    		debug("Loaded " + prismWorlds.size() + " worlds into the cache.");
		} catch (SQLException e) {
        	handleDatabaseException( e );
        } finally {
        	if(rs != null) try { rs.close(); } catch (SQLException e) {}
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
	}
	
	
	/**
	 * Saves a world name to the database, and adds the id to the cache hashmap
	 */
	public static void addWorldName( String worldName ){
		
		Connection conn = null;
		PreparedStatement s = null;
		ResultSet rs = null;
		try {

			conn = dbc();
            s = conn.prepareStatement( "INSERT INTO prism_worlds (world) VALUES (?)" , Statement.RETURN_GENERATED_KEYS);
            s.setString(1, worldName);
            s.executeUpdate();
            
            rs = s.getGeneratedKeys();
            if (rs.next()) {
                prismWorlds.put( worldName, rs.getInt(1) );
            } else {
            	throw new SQLException("Insert statement failed - no generated key obtained.");
            }
		} catch (SQLException e) {
        	
        } finally {
        	if(rs != null) try { rs.close(); } catch (SQLException e) {}
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
	}

	
	/**
	 * 
	 */
	public void cacheOnlinePlayerPrimaryKeys(){

		getServer().getScheduler().runTaskAsynchronously(this, new Runnable(){
			public void run(){
				
				String[] playerNames;
				playerNames = new String[ getServer().getOnlinePlayers().length ];
				int i = 0;
				for( Player pl : getServer().getOnlinePlayers() ){
					playerNames[i] = pl.getName();
					i++;
				}


				Connection conn = null;
				PreparedStatement s = null;
				ResultSet rs = null;
				try {
		
					conn = dbc();
		    		s = conn.prepareStatement( "SELECT player_id, player FROM prism_players WHERE player IN ('?')" );
		    		s.setString(1, TypeUtils.join(playerNames, "','"));
		    		rs = s.executeQuery();
		
		    		while( rs.next() ){
		    			debug("Loaded player " + rs.getString(2) + ", id: " + rs.getInt(1) + " into the cache.");
		    			prismPlayers.put( rs.getString(2), rs.getInt(1) );
		    		}
				} catch (SQLException e) {
		//        	handleDatabaseException( e );
		        } finally {
		        	if(rs != null) try { rs.close(); } catch (SQLException e) {}
		        	if(s != null) try { s.close(); } catch (SQLException e) {}
		        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
		        }
			}
		});
	}
	
	
	/**
	 * 
	 */
	public void cachePlayerPrimaryKey( final String playerName ){
		
//		getServer().getScheduler().runTaskAsynchronously(this, new Runnable(){
//			public void run(){

				Connection conn = null;
				PreparedStatement s = null;
				ResultSet rs = null;
				try {
		
					conn = dbc();
		    		s = conn.prepareStatement( "SELECT player_id FROM prism_players WHERE player = ?" );
		    		s.setString(1, playerName);
		    		rs = s.executeQuery();
		
		    		if( rs.next() ){
		    			debug("Loaded player " + playerName + ", id: " + rs.getInt(1) + " into the cache.");
		    			prismPlayers.put( playerName, rs.getInt(1) );
		    		} else {
		    			addPlayerName(playerName);
		    		}
				} catch (SQLException e) {
		//        	handleDatabaseException( e );
		        } finally {
		        	if(rs != null) try { rs.close(); } catch (SQLException e) {}
		        	if(s != null) try { s.close(); } catch (SQLException e) {}
		        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
		        }
//			}
//		});
	}
	
	
	/**
	 * Saves a player name to the database, and adds the id to the cache hashmap
	 */
	public static void addPlayerName( String playerName ){
		
		Connection conn = null;
		PreparedStatement s = null;
		ResultSet rs = null;
		try {

			conn = dbc();
            s = conn.prepareStatement( "INSERT INTO prism_players (player) VALUES (?)" , Statement.RETURN_GENERATED_KEYS);
            s.setString(1, playerName);
            s.executeUpdate();
            
            rs = s.getGeneratedKeys();
            if (rs.next()) {
            	debug("Saved and loaded player " + playerName + " into the cache.");
            	prismPlayers.put( playerName, rs.getInt(1) );
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
		} catch (SQLException e) {
        	
        } finally {
        	if(rs != null) try { rs.close(); } catch (SQLException e) {}
        	if(s != null) try { s.close(); } catch (SQLException e) {}
        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
	}

	
	/**
	 * 
	 * @return
	 */
	public Language getLang() {
		return this.language;
	}

	
	/**
	 * 
	 */
	public void checkPluginDependancies() {

		// HeroChat
		Plugin herochat = getServer().getPluginManager().getPlugin("Herochat");
		if (herochat != null) {
			enabledPlugins.add("Herochat");
			log("HeroChat found. Switching chat listener to HC events");
		}

		// WorldEdit
		Plugin we = getServer().getPluginManager().getPlugin("WorldEdit");
		if (we != null) {
			plugin_worldEdit = (WorldEditPlugin) we;
			PrismBlockEditSessionFactory.initialize();
			log("WorldEdit found. Associated features enabled.");
		} else {
			log("WorldEdit not found. Certain optional features of Prism disabled.");
		}
	}

	
	/**
	 * 
	 * @return
	 */
	public boolean dependencyEnabled(String pluginName) {
		return enabledPlugins.contains(pluginName);
	}
	
	
	/**
	 * 
	 * @return
	 */
	public static ArrayList<Integer> getIllegalBlocks(){
		return illegalBlocks;
	}
	
	
	/**
	 * 
	 */
	public static ArrayList<String> getIllegalEntities(){
		return illegalEntities;
	}
	
	
	/**
	 * 
	 */
	public static HashMap<String,String> getAlertedOres(){
		return alertedOres;
	}

	
	/**
	 * 
	 * @return
	 */
	public MaterialAliases getItems() {
		return this.items;
	}

	
	/**
	 * 
	 * @return
	 */
	public static ActionRegistry getActionRegistry() {
		return actionRegistry;
	}

	
	/**
	 * 
	 * @return
	 */
	public static HandlerRegistry<?> getHandlerRegistry() {
		return handlerRegistry;
	}

	
	/**
	 * 
	 * @return
	 */
	public static Ignore getIgnore() {
		return ignore;
	}

	
	/**
	 * 
	 */
	public void endExpiredQueryCaches() {
		getServer().getScheduler().scheduleSyncRepeatingTask(this,
				new Runnable() {

					public void run() {
						java.util.Date date = new java.util.Date();
						for (Map.Entry<String, QueryResult> query : cachedQueries.entrySet()) {
							QueryResult result = query.getValue();
							long diff = (date.getTime() - result.getQueryTime()) / 1000;
							if (diff >= 120) {
								cachedQueries.remove(query.getKey());
							}
						}
					}
				}, 2400L, 2400L);
	}
	

	/**
	 * 
	 */
	public void endExpiredPreviews() {
		getServer().getScheduler().scheduleSyncRepeatingTask(this,
				new Runnable() {

					public void run() {
						java.util.Date date = new java.util.Date();
						for (Map.Entry<String, PreviewSession> query : playerActivePreviews.entrySet()) {
							PreviewSession result = query.getValue();
							long diff = (date.getTime() - result.getQueryTime()) / 1000;
							if (diff >= 60) {
								// inform player
								Player player = prism.getServer().getPlayer(
										result.getPlayer().getName());
								if (player != null) {
									player.sendMessage(Prism.messenger.playerHeaderMsg("Canceling forgotten preview."));
								}
								playerActivePreviews.remove(query.getKey());
							}
						}
					}
				}, 1200L, 1200L);
	}

	
	/**
	 * 
	 */
	public void removeExpiredLocations() {
		getServer().getScheduler().scheduleSyncRepeatingTask(this,
				new Runnable() {

					public void run() {
						java.util.Date date = new java.util.Date();
						// Remove locations logged over five minute ago.
						for (Entry<Location, Long> entry : alertedBlocks.entrySet()) {
							long diff = (date.getTime() - entry.getValue()) / 1000;
							if (diff >= 300) {
								alertedBlocks.remove(entry.getKey());
							}
						}
					}
				}, 1200L, 1200L);
	}

	
	/**
	 * 
	 */
	public void actionRecorderTask() {
		int recorder_tick_delay = getConfig().getInt("prism.queue-empty-tick-delay");
		if (recorder_tick_delay < 1) {
			recorder_tick_delay = 3;
		}
		getServer().getScheduler().runTaskTimerAsynchronously(this,new ActionRecorder(prism), recorder_tick_delay,recorder_tick_delay);
	}

	
	/**
	 * 
	 */
	public void discardExpiredDbRecords() {

		List<String> purgeRules = getConfig().getStringList("prism.db-records-purge-rules");

		if (!purgeRules.isEmpty()) {

			final CopyOnWriteArrayList<QueryParameters> paramList = new CopyOnWriteArrayList<QueryParameters>();

			for (final String purgeArgs : purgeRules) {

				// Process and validate all of the arguments
				QueryParameters parameters = PreprocessArgs.process(prism,
						null, purgeArgs.split(" "), PrismProcessType.DELETE, 0, false);

				if (parameters == null) {
					log("Invalid parameters for database purge: " + purgeArgs);
					continue;
				}

				if (parameters.getFoundArgs().size() > 0) {
					parameters.setStringFromRawArgs(purgeArgs.split(" "), 0);
					paramList.add(parameters);
				}
			}

			if (paramList.size() > 0) {

				int purge_tick_delay = getConfig().getInt("prism.purge.batch-tick-delay");
				if (purge_tick_delay < 1) {
					purge_tick_delay = 20;
				}

				/**
				 * We're going to cycle through the param rules, one rule at a
				 * time in a single async task. This task will reschedule itself
				 * when each purge cycle has completed and records remain
				 */
				log("Beginning prism database purge cycle. Will be performed in batches so we don't tie up the db...");
				deleteTask = getServer().getScheduler().runTaskLaterAsynchronously(this,new PurgeTask(this, paramList,purge_tick_delay,new LogPurgeCallback()),purge_tick_delay);

			}
		}
	}

	
	/**
	 * 
	 * @param msg
	 */
	public void alertPlayers(Player player, String msg) {
		for (Player p : getServer().getOnlinePlayers()) {
			if (!p.equals(player)) {
				if (p.hasPermission("prism.alerts")) {
					p.sendMessage(messenger.playerMsg(ChatColor.RED + "[!] " + msg));
				}
			}
		}
	}

	
	/**
	 *
	 * @return
	 */
	public String msgMissingArguments() {
		return messenger.playerError("Missing arguments. Check /prism ? for help.");
	}

	
	/**
	 *
	 * @return
	 */
	public String msgInvalidArguments() {
		return messenger.playerError("Invalid arguments. Check /prism ? for help.");
	}

	
	/**
	 *
	 * @return
	 */
	public String msgInvalidSubcommand() {
		return messenger.playerError("Prism doesn't have that command. Check /prism ? for help.");
	}

	
	/**
	 *
	 * @return
	 */
	public String msgNoPermission() {
		return messenger.playerError("You don't have permission to perform this action.");
	}
	

	/**
	 * 
	 * @param player
	 * @param msg
	 */
	public void notifyNearby(Player player, int radius, String msg) {
		if (!getConfig().getBoolean("prism.appliers.notify-nearby.enabled")) {
			return;
		}
		for (Player p : player.getServer().getOnlinePlayers()) {
			if (!p.equals(player)) {
				if (player.getWorld().equals(p.getWorld())) {
					if (player.getLocation().distance(p.getLocation()) <= (radius + config.getInt("prism.appliers.notify-nearby.additional-radius"))) {
						p.sendMessage(messenger.playerHeaderMsg(msg));
					}
				}
			}
		}
	}
	

	/**
	 * 
	 * @param message
	 */
	public static void log(String message) {
		log.info("[" + getPrismName() + "]: " + message);
	}
	

	/**
	 * 
	 * @param messages
	 */
	public static void logSection(String[] messages) {
		if (messages.length > 0) {
			log("--------------------- ## Important ## ---------------------");
			for (String msg : messages) {
				log(msg);
			}
			log("--------------------- ## ========= ## ---------------------");
		}
	}

	
	/**
	 * 
	 * @param message
	 */
	public static void debug(String message) {
		if (config.getBoolean("prism.debug")) {
			log.info("[" + plugin_name + "]: " + message);
		}
	}
	

	/**
	 * 
	 * @param loc
	 */
	public static void debug(Location loc) {
		debug("Location: " + loc.getBlockX() + " " + loc.getBlockY() + " " + loc.getBlockZ());
	}
	

	/**
	 * Disable the plugin
	 */
	public void disablePlugin() {
		this.setEnabled(false);
	}
	

	/**
	 * Shutdown
	 */
	@Override
	public void onDisable() {

		// Close pool connections when plugin disables
		if (pool != null) {
			pool.close();
		}

		log("Closing plugin.");

	}
}
