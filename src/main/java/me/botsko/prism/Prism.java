package me.botsko.prism;

import io.papermc.lib.PaperLib;
import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.actionlibs.HandlerRegistry;
import me.botsko.prism.actionlibs.Ignore;
import me.botsko.prism.actionlibs.InternalAffairs;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actionlibs.QueueDrain;
import me.botsko.prism.actionlibs.RecordingTask;
import me.botsko.prism.appliers.PreviewSession;
import me.botsko.prism.commands.PrismCommands;
import me.botsko.prism.commands.WhatCommand;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.PrismDatabaseFactory;
import me.botsko.prism.listeners.PrismBlockEvents;
import me.botsko.prism.listeners.PrismCustomEvents;
import me.botsko.prism.listeners.PrismEntityEvents;
import me.botsko.prism.listeners.PrismInventoryEvents;
import me.botsko.prism.listeners.PrismInventoryMoveItemEvent;
import me.botsko.prism.listeners.PrismPlayerEvents;
import me.botsko.prism.listeners.PrismVehicleEvents;
import me.botsko.prism.listeners.PrismWorldEvents;
import me.botsko.prism.listeners.self.PrismMiscEvents;
import me.botsko.prism.measurement.QueueStats;
import me.botsko.prism.measurement.TimeTaken;
import me.botsko.prism.monitors.MetricMonitor;
import me.botsko.prism.monitors.OreMonitor;
import me.botsko.prism.monitors.UseMonitor;
import me.botsko.prism.parameters.ActionParameter;
import me.botsko.prism.parameters.BeforeParameter;
import me.botsko.prism.parameters.BlockParameter;
import me.botsko.prism.parameters.EntityParameter;
import me.botsko.prism.parameters.FlagParameter;
import me.botsko.prism.parameters.IdParameter;
import me.botsko.prism.parameters.KeywordParameter;
import me.botsko.prism.parameters.PlayerParameter;
import me.botsko.prism.parameters.PrismParameterHandler;
import me.botsko.prism.parameters.RadiusParameter;
import me.botsko.prism.parameters.SinceParameter;
import me.botsko.prism.parameters.WorldParameter;
import me.botsko.prism.players.PlayerIdentification;
import me.botsko.prism.players.PrismPlayer;
import me.botsko.prism.purge.PurgeManager;
import me.botsko.prism.utils.MaterialAliases;
import me.botsko.prism.wands.Wand;
import org.bstats.bukkit.Metrics;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.command.PluginCommand;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.scheduler.BukkitTask;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class Prism extends JavaPlugin {

    private static final HashMap<String, String> alertedOres = new HashMap<>();
    private static final Logger log = Logger.getLogger("Minecraft");
    private static final HashMap<String, PrismParameterHandler> paramHandlers = new HashMap<>();
    public static Messenger messenger;
    public static FileConfiguration config;
    public static ConcurrentHashMap<String, Wand> playersWithActiveTools = new ConcurrentHashMap<>();
    public static HashMap<String, Integer> prismWorlds = new HashMap<>();
    public static HashMap<UUID, PrismPlayer> prismPlayers = new HashMap<>();
    public static HashMap<String, Integer> prismActions = new HashMap<>();
    private static List<Material> illegalBlocks;
    private static List<EntityType> illegalEntities;
    private static PrismDataSource prismDataSource = null;
    private static String pluginName;
    private static String pasteKey;
    private static MaterialAliases items;
    private static ActionRegistry actionRegistry;
    private static HandlerRegistry handlerRegistry;
    private static Ignore ignore;
    private static Prism instance;
    private final ScheduledThreadPoolExecutor schedulePool = new ScheduledThreadPoolExecutor(1);
    private final ScheduledExecutorService recordingMonitorTask = new ScheduledThreadPoolExecutor(1);
    public boolean monitoring = false;
    public OreMonitor oreMonitor;
    public UseMonitor useMonitor;
    private MetricMonitor metricMonitor;
    public ConcurrentHashMap<String, PreviewSession> playerActivePreviews = new ConcurrentHashMap<>();
    public ConcurrentHashMap<String, ArrayList<Block>> playerActiveViews = new ConcurrentHashMap<>();
    public ConcurrentHashMap<String, QueryResult> cachedQueries = new ConcurrentHashMap<>();
    public Map<Location, Long> alertedBlocks = new ConcurrentHashMap<>();
    public TimeTaken eventTimer;
    public QueueStats queueStats;
    public BukkitTask recordingTask;
    public int totalRecordsAffected = 0;
    public long maxCycleTime = 0;
    /**
     * We store a basic index of hanging entities we anticipate will fall, so that
     * when they do fall we can attribute them to the player who broke the original
     * block.
     */
    public ConcurrentHashMap<String, String> preplannedBlockFalls = new ConcurrentHashMap<>();
    /**
     * VehicleCreateEvents do not include the player/entity that created it, so we
     * need to track players right-clicking rails with minecart vehicles, or water
     * for boats.
     */
    public ConcurrentHashMap<String, String> preplannedVehiclePlacement = new ConcurrentHashMap<>();
    private String pluginVersion;
    // private ScheduledFuture<?> scheduledPurgeExecutor;
    private PurgeManager purgeManager;

    public Prism() {
        instance = this;
    }

    public static PrismDataSource getPrismDataSource() {
        return prismDataSource;
    }

    public static String getPasteKey() {
        return pasteKey;
    }

    /**
     * Get the plugin name.
     *
     * @return String
     */
    @SuppressWarnings("WeakerAccess")
    public static String getPrismName() {
        return pluginName;
    }

    /**
     * Get a list of illegal materials.
     *
     * @return List of Blocks
     */
    public static List<Material> getIllegalBlocks() {
        return illegalBlocks;
    }

    /**
     * Get List of illegal entities.
     *
     * @return List
     */
    public static List<EntityType> getIllegalEntities() {
        return illegalEntities;
    }

    /**
     * Get List of Ores to alert.
     *
     * @return list
     */
    public static HashMap<String, String> getAlertedOres() {
        return alertedOres;
    }

    /**
     * Get material aliases.
     *
     * @return MaterialAliases
     */
    public static MaterialAliases getItems() {
        return items;
    }

    /**
     * Get the Action Registry.
     *
     * @return ActionRegistry
     */
    public static ActionRegistry getActionRegistry() {
        return actionRegistry;
    }

    /**
     * Get the HandlerRegistry.
     *
     * @return HandlerRegistry
     */
    public static HandlerRegistry getHandlerRegistry() {
        return handlerRegistry;
    }

    /**
     * Ignore.
     *
     * @return Ignore.
     */
    public static Ignore getIgnore() {
        return ignore;
    }

    /**
     * Registers a parameter and a handler. Example:
     * pr l a:block-break. The "a" is an action, and the action handler will process what
     * "block-break" refers to.
     *
     * @param handler PrismParameterHandler.
     */
    @SuppressWarnings("WeakerAccess")
    public static void registerParameter(PrismParameterHandler handler) {
        paramHandlers.put(handler.getName().toLowerCase(), handler);
    }

    /**
     * Map of Strings and PrismParameterHandlers.
     *
     * @return HashMap
     */
    public static HashMap<String, PrismParameterHandler> getParameters() {
        return paramHandlers;
    }

    /**
     * PrismParameterHandler.
     *
     * @return PrismParameterHandler
     */
    @SuppressWarnings("unused")
    public static PrismParameterHandler getParameter(String name) {
        return paramHandlers.get(name);
    }

    /**
     * Log a message.
     *
     * @param message String.
     */
    public static void log(String message) {
        log.info("[" + getPrismName() + "] " + message);
    }

    /**
     * Log a warning.
     *
     * @param message String
     */
    public static void warn(String message) {
        log.warning("[" + getPrismName() + "] " + message);
    }

    /**
     * Log a series of messages, precedent by a header.
     *
     * @param messages String[]
     */
    public static void logSection(String[] messages) {
        if (messages.length > 0) {
            log("--------------------- ## Important ## ---------------------");
            for (final String msg : messages) {
                log(msg);
            }
            log("--------------------- ## ========= ## ---------------------");
        }
    }

    /**
     * Log a debug message if config.yml has debug: true.
     *
     * @param message String
     */
    public static void debug(String message) {
        if (config == null || config.getBoolean("prism.debug")) {
            log.info("[" + pluginName + " Debug ]: " + message);
        }
    }

    /**
     * Log the current location as a debug message.
     *
     * @param loc Location.
     */
    public static void debug(Location loc) {
        debug("Location: " + loc.getX() + " " + loc.getY() + " " + loc.getZ());
    }

    public static Prism getInstance() {
        return instance;
    }

    public ScheduledThreadPoolExecutor getSchedulePool() {
        return schedulePool;
    }

    /**
     * Enables the plugin and activates our player listeners.
     */
    @Override
    public void onEnable() {

        pluginName = this.getDescription().getName();
        pluginVersion = this.getDescription().getVersion();
        log("Initializing Prism " + pluginVersion + ". Originally by Viveleroi; maintained by the AddstarMC Network");
        loadConfig();        // Load configuration, or install if new
        if (!getConfig().getBoolean("prism.suppress-paper-message", false)) {
            PaperLib.suggestPaper(this);
        }
        checkPluginDependencies();
        if (getConfig().getBoolean("prism.paste.enable")) {
            pasteKey = Prism.config.getString("prism.paste.api-key", "API KEY");
            if (pasteKey != null && (pasteKey.startsWith("API key") || pasteKey.length() < 6)) {
                pasteKey = null;
            } else {
                Prism.log("PasteApi is configured and available");
            }
        } else {
            pasteKey = null;
        }
        // init db async then call back to complete enable.
        Bukkit.getScheduler().runTaskAsynchronously(instance, () -> {
            prismDataSource = PrismDatabaseFactory.createDataSource(config);
            Connection testConnection;
            if (prismDataSource != null) {
                testConnection = prismDataSource.getConnection();
                if (testConnection != null) {
                    try {
                        testConnection.close();
                    } catch (final SQLException e) {
                        prismDataSource.handleDataSourceException(e);
                    }
                }
            } else {
                notifyDisabled();
                Bukkit.getScheduler().runTask(instance, () -> instance.onDisable());
                return;
            }
            if (testConnection == null) {
                notifyDisabled();
                Bukkit.getScheduler().runTask(instance, () -> instance.onDisable());
                return;
            }
            Bukkit.getScheduler().runTask(instance, () -> instance.enabled());
        });
    }

    private void notifyDisabled() {
        final String[] dbDisabled = new String[3];
        dbDisabled[0] = "Prism will disable itself because it couldn't connect to a database.";
        dbDisabled[1] = "If you're using MySQL, check your config. Be sure MySQL is running.";
        dbDisabled[2] = "For help - try our Discord Channel or the Wiki on Github.";
        logSection(dbDisabled);
    }

    private void enabled() {
        if (isEnabled()) {

            // Info needed for setup, init these here
            handlerRegistry = new HandlerRegistry();
            actionRegistry = new ActionRegistry();

            // Setup databases
            prismDataSource.setupDatabase(actionRegistry);

            // Cache world IDs
            prismDataSource.cacheWorldPrimaryKeys(prismWorlds);
            PlayerIdentification.cacheOnlinePlayerPrimaryKeys();

            // ensure current worlds are added
            for (final World w : getServer().getWorlds()) {
                if (!Prism.prismWorlds.containsKey(w.getName())) {
                    prismDataSource.addWorldName(w.getName());
                }
            }

            // Apply any updates
            final Updater up = new Updater(this);
            up.applyUpdates();

            eventTimer = new TimeTaken(this);
            queueStats = new QueueStats();
            ignore = new Ignore(this);

            // Assign event listeners
            getServer().getPluginManager().registerEvents(new PrismBlockEvents(this), this);
            getServer().getPluginManager().registerEvents(new PrismEntityEvents(this), this);
            getServer().getPluginManager().registerEvents(new PrismWorldEvents(), this);
            getServer().getPluginManager().registerEvents(new PrismPlayerEvents(this), this);
            getServer().getPluginManager().registerEvents(new PrismInventoryEvents(this), this);
            getServer().getPluginManager().registerEvents(new PrismVehicleEvents(this), this);

            // InventoryMoveItem
            if (getConfig().getBoolean("prism.track-hopper-item-events") && Prism.getIgnore().event("item-insert")) {
                getServer().getPluginManager().registerEvents(new PrismInventoryMoveItemEvent(), this);
            }

            if (getConfig().getBoolean("prism.tracking.api.enabled")) {
                getServer().getPluginManager().registerEvents(new PrismCustomEvents(this), this);
            }

            // Assign listeners to our own events
            // getServer().getPluginManager().registerEvents(new
            // PrismRollbackEvents(), this);
            getServer().getPluginManager().registerEvents(new PrismMiscEvents(), this);

            // Add commands
            PluginCommand command = getCommand("prism");
            if (command != null) {
                PrismCommands commands = new PrismCommands(this);
                command.setExecutor(commands);
                command.setTabCompleter(commands);
            } else {
                warn("Command Executor Error: Check plugin.yml");
                Bukkit.getPluginManager().disablePlugin(instance);
                return;
            }
            PluginCommand commandAlt = getCommand("what");
            if (commandAlt != null) {
                commandAlt.setExecutor(new WhatCommand(this));
            } else {
                log("Command Executor Error: Check plugin.yml - what command not found ");
            }
            // Register official parameters
            registerParameter(new ActionParameter());
            registerParameter(new BeforeParameter());
            registerParameter(new BlockParameter());
            registerParameter(new EntityParameter());
            registerParameter(new FlagParameter());
            registerParameter(new IdParameter());
            registerParameter(new KeywordParameter());
            registerParameter(new PlayerParameter());
            registerParameter(new RadiusParameter());
            registerParameter(new SinceParameter());
            registerParameter(new WorldParameter());

            // Init re-used classes
            messenger = new Messenger(pluginName);
            oreMonitor = new OreMonitor(instance);
            useMonitor = new UseMonitor(instance);

            // Init async tasks
            actionRecorderTask();

            // Init scheduled events
            endExpiredQueryCaches();
            endExpiredPreviews();
            removeExpiredLocations();

            // Delete old data based on config
            launchScheduledPurgeManager();

            // Keep watch on db connections, other sanity
            launchInternalAffairs();

            if (config.getBoolean("prism.preload-materials")) {
                config.set("prism.preload-materials", false);
                saveConfig();
                Prism.log("Preloading materials - This will take a while!");

                items.initAllMaterials();
                Prism.log("Preloading complete!");
            }

            items.initMaterials(Material.AIR);
        }
    }

    /**
     * The version of Prism.
     *
     * @return String
     */
    public String getPrismVersion() {
        return this.pluginVersion;
    }

    /**
     * Load configuration and language files.
     */
    public void loadConfig() {
        final PrismConfig mc = new PrismConfig(this);
        config = mc.getConfig();

        // Cache config arrays we check constantly
        illegalBlocks = getConfig().getStringList("prism.appliers.never-place-block").stream()
                .map(Material::matchMaterial).filter(Objects::nonNull).collect(Collectors.toList());
        illegalEntities = getConfig().getStringList("prism.appliers.never-spawn-entity")
                .stream()
                .map(s -> {
                    try {
                        return EntityType.valueOf(s.toUpperCase());
                    } catch (Exception e) {
                        debug(e.getMessage());
                    }
                    return null;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        final ConfigurationSection alertBlocks = getConfig()
                .getConfigurationSection("prism.alerts.ores.blocks");
        alertedOres.clear();
        if (alertBlocks != null) {
            for (final String key : alertBlocks.getKeys(false)) {
                alertedOres.put(key.toUpperCase(), alertBlocks.getString(key));
            }
        }
        items = new MaterialAliases();
    }

    private void checkPluginDependencies() {
        //DripReporter
        ApiHandler.configureMonitor();
        // WorldEdit
        ApiHandler.hookWorldEdit();
        //bstats
        metricMonitor = new MetricMonitor();
    }

    /**
     * Check if a dependency so names is available.
     *
     * @return true
     */
    public boolean dependencyEnabled(String pluginName) {
        return ApiHandler.checkDependency(pluginName);
    }

    /**
     * PurgeManager.
     *
     * @return PurgeManager
     */
    public PurgeManager getPurgeManager() {
        return purgeManager;
    }

    /**
     * Clears the Query Cache.
     */
    @SuppressWarnings("WeakerAccess")
    public void endExpiredQueryCaches() {
        getServer().getScheduler().scheduleSyncRepeatingTask(this, () -> {
            final java.util.Date date = new java.util.Date();
            for (final Entry<String, QueryResult> query : cachedQueries.entrySet()) {
                final QueryResult result = query.getValue();
                final long diff = (date.getTime() - result.getQueryTime()) / 1000;
                if (diff >= 120) {
                    cachedQueries.remove(query.getKey());
                }
            }
        }, 2400L, 2400L);
    }

    /**
     * Clears expired Previews.
     */
    @SuppressWarnings("WeakerAccess")
    public void endExpiredPreviews() {
        getServer().getScheduler().scheduleSyncRepeatingTask(this, () -> {
            final java.util.Date date = new java.util.Date();
            for (final Entry<String, PreviewSession> query : playerActivePreviews.entrySet()) {
                final PreviewSession result = query.getValue();
                final long diff = (date.getTime() - result.getQueryTime()) / 1000;
                if (diff >= 60) {
                    // inform player

                    final Player player = result.getPlayer();
                    if (player.isOnline()) {
                        player.sendMessage(Prism.messenger.playerHeaderMsg("Canceling forgotten preview."));
                    }
                    playerActivePreviews.remove(query.getKey());
                }
            }
        }, 1200L, 1200L);
    }

    /**
     * Remove expired locations.
     */
    @SuppressWarnings("WeakerAccess")
    public void removeExpiredLocations() {
        getServer().getScheduler().scheduleSyncRepeatingTask(this, () -> {
            final java.util.Date date = new java.util.Date();
            // Remove locations logged over five minute ago.
            for (final Entry<Location, Long> entry : alertedBlocks.entrySet()) {
                final long diff = (date.getTime() - entry.getValue()) / 1000;
                if (diff >= 300) {
                    alertedBlocks.remove(entry.getKey());
                }
            }
        }, 1200L, 1200L);
    }

    /**
     * Schedule the RecorderTask async.
     */
    public void actionRecorderTask() {
        int recorderTickDelay = getConfig().getInt("prism.queue-empty-tick-delay");
        if (recorderTickDelay < 1) {
            recorderTickDelay = 3;
        }
        // we schedule it once, it will reschedule itself
        recordingTask = getServer().getScheduler().runTaskLaterAsynchronously(this, new RecordingTask(this),
                recorderTickDelay);
    }

    /**
     * Schedule the Purge manager.
     */
    private void launchScheduledPurgeManager() {
        final List<String> purgeRules = getConfig().getStringList("prism.db-records-purge-rules");
        purgeManager = new PurgeManager(this, purgeRules);
        schedulePool.scheduleAtFixedRate(purgeManager, 0, 12, TimeUnit.HOURS);
    }

    /**
     * Launch InternalAffairs - to monitor recording.
     */
    private void launchInternalAffairs() {
        final Runnable recordingMonitor = new InternalAffairs(this);
        recordingMonitorTask.scheduleAtFixedRate(recordingMonitor, 0, 5, TimeUnit.MINUTES);
    }

    /**
     * Send an alert to a player.
     *
     * @param msg String
     */
    public void alertPlayers(Player player, String msg) {
        for (final Player p : getServer().getOnlinePlayers()) {
            if (!p.equals(player) || getConfig().getBoolean("prism.alerts.alert-player-about-self")) {
                if (p.hasPermission("prism.alerts")) {
                    p.sendMessage(messenger.playerMsg(ChatColor.RED + "[!] " + msg));
                }
            }
        }
    }

    /**
     * Report nearby players of a set radius.
     *
     * @param player Player
     * @param radius int
     * @param msg    String
     */
    public void notifyNearby(Player player, int radius, String msg) {
        if (!getConfig().getBoolean("prism.appliers.notify-nearby.enabled")) {
            return;
        }
        int distance = (radius
                + config.getInt("prism.appliers.notify-nearby.additional-radius")) ^ 2;
        for (final Player p : player.getServer().getOnlinePlayers()) {
            if (!p.getUniqueId().equals(player.getUniqueId())
                    && player.getWorld().equals(p.getWorld())
                    && player.getLocation().distanceSquared(p.getLocation()) <= distance) {
                p.sendMessage(messenger.playerHeaderMsg(msg));
            }
        }
    }

    /**
     * Shutdown.
     */
    @Override
    public void onDisable() {
        if (getConfig().getBoolean("prism.database.force-write-queue-on-shutdown")) {
            final QueueDrain drainer = new QueueDrain(this);
            drainer.forceDrainQueue();
        }
        if (!ApiHandler.disable()) {
            log("Possible errors unhooking dependencies...");
        }
        metricMonitor.disable();
        Bukkit.getScheduler().cancelTasks(this);
        // Close prismDataSource connections when plugin disables
        if (prismDataSource != null) {
            prismDataSource.dispose();
        }
        log("Closing plugin.");
        super.onDisable();
    }
}
