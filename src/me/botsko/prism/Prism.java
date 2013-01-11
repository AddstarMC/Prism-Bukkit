package me.botsko.prism;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import me.botsko.prism.actionlibs.ActionRecorder;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.PreviewSession;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commands.PrismCommands;
import me.botsko.prism.db.Mysql;
import me.botsko.prism.listeners.PrismBlockEvents;
import me.botsko.prism.listeners.PrismEntityEvents;
import me.botsko.prism.listeners.PrismInventoryEvents;
import me.botsko.prism.listeners.PrismPlayerEvents;
import me.botsko.prism.listeners.PrismWorldEvents;
import me.botsko.prism.listeners.self.PrismRollbackEvents;
import me.botsko.prism.monitors.OreMonitor;
import me.botsko.prism.wands.Wand;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.command.CommandExecutor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.plugin.java.JavaPlugin;

public class Prism extends JavaPlugin {

	protected String msg_name = "Prism";
	public Prism prism;
	protected Logger log = Logger.getLogger("Minecraft");
	public FileConfiguration config;
	protected Language language;
	protected MaterialAliases items;
	public Connection conn = null;
	
	public ActionRecorder actionsRecorder;
	public ActionsQuery actionsQuery;
	public OreMonitor oreMonitor;
	public ConcurrentHashMap<String,Wand> playersWithActiveTools = new ConcurrentHashMap<String,Wand>();
	public ConcurrentHashMap<String,PreviewSession> playerActivePreviews = new ConcurrentHashMap<String,PreviewSession>();
	public ConcurrentHashMap<String, QueryResult> cachedQueries = new ConcurrentHashMap<String,QueryResult>();
	public ConcurrentHashMap<Location,Long> alertedBlocks = new ConcurrentHashMap<Location,Long>();
	
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
		
		prism = this;
		
		this.log("Initializing plugin. By Viveleroi (and team), Darkhelmet Minecraft: s.dhmc.us");
		
//		try {
//		    Metrics metrics = new Metrics(this);
//		    metrics.start();
//		} catch (IOException e) {
//		    log("MCStats submission failed.");
//		}
		
		// Load configuration, or install if new
		loadConfig();
		
		// Setup databases
		setupDatabase();
		
		// Assign event listeners
		getServer().getPluginManager().registerEvents(new PrismBlockEvents( this ), this);
		getServer().getPluginManager().registerEvents(new PrismEntityEvents( this ), this);
		getServer().getPluginManager().registerEvents(new PrismWorldEvents( this ), this);
		getServer().getPluginManager().registerEvents(new PrismPlayerEvents( this ), this);
		getServer().getPluginManager().registerEvents(new PrismInventoryEvents( this ), this);
		
		// Assign listeners to our own events
		getServer().getPluginManager().registerEvents(new PrismRollbackEvents( this ), this);
		
		// Add commands
		getCommand("prism").setExecutor( (CommandExecutor) new PrismCommands(this) );
		
		// Init re-used classes
		actionsRecorder = new ActionRecorder(this);
		actionsQuery = new ActionsQuery(this);
		oreMonitor = new OreMonitor(this);
		
		// Init async tasks
		actionRecorderTask();
		
		// Init scheduled events
		endExpiredQueryCaches();
		endExpiredPreviews();
		removeExpiredLocations();
		
		// Delete old data based on config
		discardExpiredDbRecords();
		
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
     * Setup a generic connection all non-scheduled methods may share
	 * @throws ClassNotFoundException
	 * @throws IllegalAccessException
	 * @throws InstantiationException
	 * @return true if we successfully connected to the db.
     */
	public void dbc(){
		Mysql mysql = new Mysql(
				config.getString("prism.mysql.username"), 
				config.getString("prism.mysql.password"), 
				config.getString("prism.mysql.hostname"), 
				config.getString("prism.mysql.database"), 
				config.getString("prism.mysql.port")
		);
		conn = mysql.getConn();
	}
	
	
	/**
	 * 
	 */
	protected void setupDatabase(){

		try{
	        dbc();
	        String query = "CREATE TABLE IF NOT EXISTS `prism_actions` (" +
	        		"`id` int(11) unsigned NOT NULL auto_increment," +
	        		"`action_time` datetime NOT NULL," +
	        		"`action_type` varchar(25) NOT NULL," +
	        		"`player` varchar(16) NOT NULL," +
	        		"`world` varchar(255) NOT NULL," +
	        		"`x` int(11) NOT NULL," +
	        		"`y` int(11) NOT NULL," +
	        		"`z` int(11) NOT NULL," +
	        		"`data` varchar(255) NOT NULL," +
	        		"PRIMARY KEY  (`id`)" +
	        		") ENGINE=MyISAM  DEFAULT CHARSET=latin1;";
	        
            Statement st = conn.createStatement();
            st.executeUpdate(query);
            conn.close();
	    }
	    catch (SQLException e){
	        e.printStackTrace();
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
		    		if(diff >= 300){
		    			cachedQueries.remove(query.getKey());
		    		}
		    	}
		    }
		}, 6000L, 6000L);
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
		getServer().getScheduler().scheduleSyncRepeatingTask(this, new ActionRecorder(prism), 3L, 3L);
	}
	
	
	/**
	 * 
	 */
	public void discardExpiredDbRecords(){
		
		String dateBefore = PreprocessArgs.translateTimeStringToDate( this, null, getConfig().getString("prism.clear-records-after") );
		if(dateBefore != null && !dateBefore.isEmpty()){
			ActionsQuery aq = new ActionsQuery(this);
			int rows_affected = aq.delete(dateBefore);
			log("Clearing " + rows_affected + " rows from the database. Older than " + getConfig().getString("prism.clear-records-after"));
		}
	}
	
	
	/**
	 * 
	 * @param msg
	 * @return
	 */
	public String playerHeaderMsg(String msg){
		if(msg != null){
			return ChatColor.LIGHT_PURPLE + msg_name+" // " + ChatColor.WHITE + msg;
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
			return ChatColor.LIGHT_PURPLE + msg_name+" // " + ChatColor.GRAY + msg;
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
			return ChatColor.LIGHT_PURPLE + msg_name+" // " + ChatColor.RED + msg;
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
		log.info("["+msg_name+"]: " + message);
	}
	
	
	/**
	 * 
	 * @param message
	 */
	public void debug(String message){
		if(this.config.getBoolean("prism.debug")){
			log.info("["+msg_name+"]: " + message);
		}
	}
	
	
	/**
	 * Shutdown
	 */
	@Override
	public void onDisable(){
		this.log("Closing plugin.");
	}
}
