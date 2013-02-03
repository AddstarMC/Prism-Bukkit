package me.botsko.prism;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import me.botsko.prism.actionlibs.ActionRecorder;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.PreviewSession;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.bridge.PrismBlockEditSessionFactory;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commands.PrismCommands;
import me.botsko.prism.db.MySQL;
import me.botsko.prism.db.Updater;
import me.botsko.prism.listeners.PrismBlockEvents;
import me.botsko.prism.listeners.PrismEntityEvents;
import me.botsko.prism.listeners.PrismInventoryEvents;
import me.botsko.prism.listeners.PrismPlayerEvents;
import me.botsko.prism.listeners.PrismWorldEvents;
import me.botsko.prism.listeners.self.PrismMiscEvents;
import me.botsko.prism.measurement.Metrics;
import me.botsko.prism.measurement.QueueStats;
import me.botsko.prism.measurement.TimeTaken;
import me.botsko.prism.monitors.OreMonitor;
import me.botsko.prism.monitors.UseMonitor;
import me.botsko.prism.wands.Wand;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.command.CommandExecutor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.java.JavaPlugin;

import com.sk89q.worldedit.bukkit.WorldEditPlugin;

public class Prism extends JavaPlugin {

	/**
	 * Protected/private
	 */
	protected String plugin_name;
	protected String plugin_version;
	protected MaterialAliases items;
	protected Language language;
	protected Logger log = Logger.getLogger("Minecraft");
	
	/**
	 * Public
	 */
	public Prism prism;
	public FileConfiguration config;
	public Connection conn = null;
	public WorldEditPlugin plugin_worldEdit = null;
	public static ActionRecorder actionsRecorder;
	public ActionsQuery actionsQuery;
	public OreMonitor oreMonitor;
	public UseMonitor useMonitor;
	public ConcurrentHashMap<String,Wand> playersWithActiveTools = new ConcurrentHashMap<String,Wand>();
	public ConcurrentHashMap<String,PreviewSession> playerActivePreviews = new ConcurrentHashMap<String,PreviewSession>();
	public ConcurrentHashMap<String, QueryResult> cachedQueries = new ConcurrentHashMap<String,QueryResult>();
	public ConcurrentHashMap<Location,Long> alertedBlocks = new ConcurrentHashMap<Location,Long>();
	public TimeTaken eventTimer;
	public QueueStats queueStats;

	
	/**
	 * We store a basic index of blocks we anticipate will fall, so
	 * that when they do fall we can attribute them to the player who
	 * broke the original block.
	 * 
	 * Once the block fall is registered, it's removed from here, so
	 * data should not remain here long.
	 */
	public ConcurrentHashMap<String,String> preplannedBlockFalls = new ConcurrentHashMap<String,String>();
	
	
    /**
     * Enables the plugin and activates our player listeners
     */
	@Override
	public void onEnable(){
		
		plugin_name = this.getDescription().getName();
		plugin_version = this.getDescription().getVersion();

		prism = this;
		
		this.log("Initializing Prism " + plugin_version + ". By Viveleroi.");
		
		if(getConfig().getBoolean("prism.notify-newer-versions")){
			String notice = UpdateNotification.checkForNewerBuild(plugin_version);
			if(notice != null){
				log(notice);
			}
		}
		
		try {
		    Metrics metrics = new Metrics(this);
		    metrics.start();
		} catch (IOException e) {
		    log("MCStats submission failed.");
		}
		
		// Load configuration, or install if new
		loadConfig();
		
		// init db
		dbc();
		if( conn == null ){
			
			String[] dbDisabled = new String[3];
			dbDisabled[0] = "Prism will disable itself because it couldn't connect to a database.";
			dbDisabled[1] = "If you're using MySQL, check your config. Be sure MySQL is running.";
			dbDisabled[2] = "For help - try http://discover-prism.com/wiki/view/troubleshooting/";
			logSection(dbDisabled);
			
			disablePlugin();
		}

		if(isEnabled()){
		
			// Setup databases
			setupDatabase();
			
			// Apply any updates
			Updater up = new Updater(this);
			up.apply_updates();
			
			eventTimer = new TimeTaken();
			queueStats = new QueueStats();
			
			// Plugins we use
			checkPluginDependancies();
			
			// Assign event listeners
			getServer().getPluginManager().registerEvents(new PrismBlockEvents( this ), this);
			getServer().getPluginManager().registerEvents(new PrismEntityEvents( this ), this);
			getServer().getPluginManager().registerEvents(new PrismWorldEvents(), this);
			getServer().getPluginManager().registerEvents(new PrismPlayerEvents( this ), this);
			getServer().getPluginManager().registerEvents(new PrismInventoryEvents(), this);
			
			// Assign listeners to our own events
	//		getServer().getPluginManager().registerEvents(new PrismRollbackEvents(), this);
			getServer().getPluginManager().registerEvents(new PrismMiscEvents(), this);
			
			// Add commands
			getCommand("prism").setExecutor( (CommandExecutor) new PrismCommands(this) );
			
			// Init re-used classes
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
	public String getPrismVersion(){
		return this.plugin_version;
	}
	
	
	/**
	 * Load configuration and language files
	 */
	public void loadConfig(){
		PrismConfig mc = new PrismConfig( this );
		config = mc.getConfig();
		// Load language files
		language = new Language( this, mc.getLang() );
		// Load items db
		items = new MaterialAliases( mc.getItems() );
	}
	
	
	/**
	 * 
	 */
	public void dbc(){
		
		// SQLITE
		if( getConfig().getString("prism.database.mode").equalsIgnoreCase("sqlite") ){
        	try {
				if( conn == null || conn.isClosed() ){
					conn = getDbConnection();
				}
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
		
		// MYSQL
		else if( getConfig().getString("prism.database.mode").equalsIgnoreCase("mysql") ){
			conn = getDbConnection();
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Connection getDbConnection(){
		Connection connection = null;
		// SQLITE
		if( getConfig().getString("prism.database.mode").equalsIgnoreCase("sqlite") ){
	        try {
	        	Class.forName("org.sqlite.JDBC");
	        	connection = DriverManager.getConnection("jdbc:sqlite:plugins/Prism/Prism.db");
	        	Statement st = connection.createStatement();
				st.executeUpdate("PRAGMA journal_mode = WAL;");
				st.close();
			} catch (SQLException e) {
				this.log("Error: SQLite database connection was not established. " + e.getMessage());
			} catch (ClassNotFoundException e) {
				this.log("Error: SQLite database connection was not established. " + e.getMessage());
			}
		}
		
		// MYSQL
		else if( getConfig().getString("prism.database.mode").equalsIgnoreCase("mysql") ){
			MySQL mysql = new MySQL(
							config.getString("prism.mysql.username"),
							config.getString("prism.mysql.password"),
							config.getString("prism.mysql.hostname"),
							config.getString("prism.mysql.database"),
							config.getString("prism.mysql.port"));
			connection = mysql.getConn();
			if(connection == null){
				this.log("Error: MySQL database connection was not established. Please check your configuration file.");
			}
		}
		return connection;
	}
	
	
	/**
	 * 
	 */
	protected void setupDatabase(){

		// SQLITE
		if( getConfig().getString("prism.database.mode").equalsIgnoreCase("sqlite") ){
			
			 try {
				 String query = "CREATE TABLE IF NOT EXISTS `prism_actions` (" +
			        		"id INT PRIMARY KEY," +
			        		"action_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP," +
			        		"action_type TEXT," +
			        		"player TEXT," +
			        		"world TEXT," +
			        		"x INT," +
			        		"y INT," +
			        		"z INT," +
			        		"data TEXT" +
			        		")";
					Statement st = conn.createStatement();
					st.executeUpdate(query);
					st.executeUpdate("CREATE INDEX IF NOT EXISTS x ON prism_actions (x ASC)");
					st.executeUpdate("CREATE INDEX IF NOT EXISTS action_type ON prism_actions (action_type ASC)");
					st.executeUpdate("CREATE INDEX IF NOT EXISTS player ON prism_actions (player ASC)");
					
					query = "CREATE TABLE IF NOT EXISTS `prism_meta` (" +
			        		"id INT PRIMARY KEY," +
			        		"k TEXT," +
			        		"v TEXT" +
			        		")";
					st.executeUpdate(query);
					
					st.close();
					conn.close();
			 }
			 catch(SQLException e){
				 log("Database connection error: " + e.getMessage());
			     e.printStackTrace();
			 }
		}
		
		// MYSQL
		else if( getConfig().getString("prism.database.mode").equalsIgnoreCase("mysql") ){
			try{
		        dbc();
		        if(conn == null) return;
		        String query = "CREATE TABLE IF NOT EXISTS `prism_actions` (" +
		        		"`id` int(11) unsigned NOT NULL auto_increment," +
		        		"`action_time` timestamp NOT NULL default CURRENT_TIMESTAMP," +
		        		"`action_type` varchar(25) NOT NULL," +
		        		"`player` varchar(16) NOT NULL," +
		        		"`world` varchar(255) NOT NULL," +
		        		"`x` int(11) NOT NULL," +
		        		"`y` int(11) NOT NULL," +
		        		"`z` int(11) NOT NULL," +
		        		"`data` varchar(255) NOT NULL," +
		        		"PRIMARY KEY  (`id`), " +
		        		"KEY `x` (`x`)" +
		        		") ENGINE=MyISAM;";
		        
	            Statement st = conn.createStatement();
	            st.executeUpdate(query);
	            
	            query = "CREATE TABLE IF NOT EXISTS `prism_meta` (" +
	            		"`id` int(10) unsigned NOT NULL auto_increment," +
	            		"`k` varchar(25) NOT NULL," +
	            		"`v` varchar(255) NOT NULL," +
	            		"PRIMARY KEY  (`id`)" +
	            		") ENGINE=MyISAM DEFAULT CHARSET=latin1;";
	            st.executeUpdate(query);
	            
	            st.close();
	            conn.close();
		    }
		    catch (SQLException e){
		    	log("Database connection error: " + e.getMessage());
		        e.printStackTrace();
		    }
		}
	}

	
	/**
	 * 
	 * @return
	 */
	public Language getLang(){
		return this.language;
	}
	
	
	/**
	 * 
	 */
	public void checkPluginDependancies(){
		Plugin we = getServer().getPluginManager().getPlugin("WorldEdit");
		if (we != null) {
			plugin_worldEdit = (WorldEditPlugin)we;
			PrismBlockEditSessionFactory.initialize();
			log("WorldEdit found. Associated features enabled.");
		}
		else {
			log("WorldEdit not found. Certain optional features of Prism disabled.");
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public MaterialAliases getItems(){
		return this.items;
	}
	
	
	/**
	 * 
	 */
	public void endExpiredQueryCaches(){
		getServer().getScheduler().scheduleSyncRepeatingTask(this, new Runnable() {

		    public void run() {
		    	java.util.Date date = new java.util.Date();
		    	for (Map.Entry<String, QueryResult> query : cachedQueries.entrySet()){
		    		QueryResult result = query.getValue();
		    		long diff = (date.getTime() - result.getQueryTime()) / 1000;
		    		if(diff >= 120){
		    			cachedQueries.remove(query.getKey());
		    		}
		    	}
		    }
		}, 2400L, 2400L);
	}
	
	
	/**
	 * 
	 */
	public void endExpiredPreviews(){
		getServer().getScheduler().scheduleSyncRepeatingTask(this, new Runnable() {

		    public void run() {
		    	java.util.Date date = new java.util.Date();
		    	for (Map.Entry<String, PreviewSession> query : playerActivePreviews.entrySet()){
		    		PreviewSession result = query.getValue();
		    		long diff = (date.getTime() - result.getQueryTime()) / 1000;
		    		if(diff >= 60){
		    			// inform player
		    			Player player = prism.getServer().getPlayer(result.getPlayer().getName());
		    			if(player != null){
		    				player.sendMessage( prism.playerHeaderMsg("Canceling forgotten preview.") );
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
	public void removeExpiredLocations(){
		getServer().getScheduler().scheduleSyncRepeatingTask(this, new Runnable() {

		    public void run() {
		    	java.util.Date date = new java.util.Date();
		    	// Remove locations logged over five minute ago.
		    	for (Entry<Location, Long> entry : alertedBlocks.entrySet()){
		    		long diff = (date.getTime() - entry.getValue()) / 1000;
		    		if(diff >= 300){
		    			alertedBlocks.remove(entry.getKey());
		    		}
		    	}
		    }
		}, 1200L, 1200L);
	}
	
	
	/**
	 * 
	 */
	public void actionRecorderTask(){
		getServer().getScheduler().runTaskTimerAsynchronously(this, new ActionRecorder(prism), 3L, 3L);
	}
	
	
	/**
	 * 
	 */
	@SuppressWarnings("unchecked")
	public void discardExpiredDbRecords(){
		
		List<String> purgeRules = (List<String>) getConfig().getList("prism.db-records-purge-rules");
		
		if(!purgeRules.isEmpty()){
			
			final ArrayList<QueryParameters> paramList = new ArrayList<QueryParameters>();
			
			for(final String purgeArgs : purgeRules){

				// Process and validate all of the arguments
				QueryParameters parameters = PreprocessArgs.process( prism, null, purgeArgs.split(" "), PrismProcessType.DELETE, 0 );
				
				if(parameters == null){
					log("Invalid parameters for database purge: " + purgeArgs);
					continue;
				}
				if(parameters.getFoundArgs().size() > 0){
					parameters.setStringFromRawArgs( purgeArgs.split(" "), 0 );
					paramList.add( parameters );
				}
			}
			
			if(paramList.size() > 0){
				getServer().getScheduler().runTaskAsynchronously(this, new Runnable(){
				    public void run(){
				    	for(QueryParameters param : paramList){
							ActionsQuery aq = new ActionsQuery(prism);
							int rows_affected = aq.delete(param);
							log("Clearing " + rows_affected + " rows from the database. Using:" + param.getOriginalCommand() );
				    	}
				    }
				});
			}
		}
	}
	
	
	/**
	 * 
	 * @param msg
	 * @return
	 */
	public String playerHeaderMsg(String msg){
		if(msg != null){
			return ChatColor.LIGHT_PURPLE + plugin_name+" // " + ChatColor.WHITE + msg;
		}
		return "";
	}
	
	
	/**
	 * 
	 * @param msg
	 * @return
	 */
	public String playerSubduedHeaderMsg(String msg){
		if(msg != null){
			return ChatColor.LIGHT_PURPLE + plugin_name+" // " + ChatColor.GRAY + msg;
		}
		return "";
	}
	
	
	/**
	 * 
	 * @param msg
	 * @return
	 */
	public String playerMsg(String msg){
		if(msg != null){
			return ChatColor.WHITE + msg;
		}
		return "";
	}
	
	
	/**
	 * 
	 * @param player_name
	 * @param cmd
	 * @param help
	 */
	public String playerHelp( String cmd, String help ){
		return ChatColor.GRAY + "/prism " + ChatColor.LIGHT_PURPLE + cmd + ChatColor.WHITE + " - " + help;
	}
	
	
	/**
	 * 
	 * @param msg
	 * @return
	 */
	public String playerError(String msg){
		if(msg != null){
			return ChatColor.LIGHT_PURPLE + plugin_name+" // " + ChatColor.RED + msg;
		}
		return "";
	}
	
	
	/**
	 * 
	 * @param msg
	 */
	public void alertPlayers( Player player, String msg ){
		for (Player p : getServer().getOnlinePlayers()) {
			if( !p.equals( player ) ){
				if (p.hasPermission("prism.alerts")){
					p.sendMessage( playerMsg( ChatColor.RED+ "[!] "+msg ) );
				}
			}
		}
	}
	
	
	/**
	 * 
	 * @param msg
	 * @return
	 */
	public String msgMissingArguments(){
		return playerError("Missing arguments. Check /prism ? for help.");
	}
	
	
	/**
	 * 
	 * @param msg
	 * @return
	 */
	public String msgInvalidArguments(){
		return playerError("Invalid arguments. Check /prism ? for help.");
	}
	
	
	/**
	 * 
	 * @param msg
	 * @return
	 */
	public String msgInvalidSubcommand(){
		return playerError("Prism doesn't have that command. Check /prism ? for help.");
	}
	
	
	/**
	 * 
	 * @param msg
	 * @return
	 */
	public String msgNoPermission(){
		return playerError("You don't have permission to perform this action.");
	}
	
	
	/**
	 * 
	 * @param player
	 * @param msg
	 */
	public void notifyNearby( Player player, int radius, String msg ) {
		if(!getConfig().getBoolean("prism.appliers.notify-nearby.enabled")){
			return;
		}
        for (Player p : player.getServer().getOnlinePlayers()) {
        	if( !p.equals( player ) ){
        		if(player.getWorld().equals(p.getWorld())){
		        	if(player.getLocation().distance( p.getLocation() ) <= (radius+config.getInt("prism.appliers.notify-nearby.additional-radius"))){
		                p.sendMessage(playerHeaderMsg(msg));
		        	}
        		}
        	}
        }
    }

	
	/**
	 * 
	 * @param message
	 */
	public void log(String message){
		log.info("["+plugin_name+"]: " + message);
	}
	
	
	/**
	 * 
	 * @param message
	 */
	public void logSection(String[] messages){
		if(messages.length > 0){
			log("--------------------- ## Important ## ---------------------");
			for(String msg : messages){
				log(msg);
			}
			log("--------------------- ## ========= ## ---------------------");
		}
	}
	
	
	/**
	 * 
	 * @param message
	 */
	public void debug(String message){
		if(this.config.getBoolean("prism.debug")){
			log.info("["+plugin_name+"]: " + message);
		}
	}
	
	
	/**
	 * Disable the plugin
	 */
	public void disablePlugin(){
		this.setEnabled(false);
	}
	
	
	/**
	 * Shutdown
	 */
	@Override
	public void onDisable(){
		this.log("Closing plugin.");
	}
}
