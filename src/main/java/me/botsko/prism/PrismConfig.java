package me.botsko.prism;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.plugin.Plugin;

public class PrismConfig extends ConfigBase {

    /**
     * 
     * @param plugin
     */
    public PrismConfig(Plugin plugin) {
        super( plugin );
    }

    /**
	 *
	 */
    @Override
    public FileConfiguration getConfig() {

        config = plugin.getConfig();

        // set defaults
        config.addDefault( "prism.debug", false );
        // config.addDefault("prism.language", "en-us");

        config.addDefault( "prism.allow-metrics", true );

        // Database
        config.addDefault( "prism.database.max-pool-connections", 20 );
        config.addDefault( "prism.database.pool-initial-size", 10 );
        config.addDefault( "prism.database.max-idle-connections", 10 );
        config.addDefault( "prism.database.max-wait", 30000 );
        config.addDefault( "prism.database.max-failures-before-wait", 5 );
        config.addDefault( "prism.database.actions-per-insert-batch", 1000 );

        // queue
        config.addDefault( "prism.database.force-write-queue-on-shutdown", true );

        // Mysql
        config.addDefault( "prism.mysql.hostname", "127.0.0.1" );
        config.addDefault( "prism.mysql.username", "root" );
        config.addDefault( "prism.mysql.password", "" );
        config.addDefault( "prism.mysql.database", "minecraft" );
        config.addDefault( "prism.mysql.prefix", "prism_" );
        config.addDefault( "prism.mysql.port", "3306" );

        // pste.me sharing.
        config.addDefault( "prism.paste.enable", false );
        config.addDefault( "prism.paste.username", "Username on http://pste.me/#/signup" );
        config.addDefault( "prism.paste.api-key", "API key from http://pste.me/#/account" );

        // Wands
        config.addDefault( "prism.wands.default-mode", "hand" ); // hand, item,
                                                                 // or block
        config.addDefault( "prism.wands.default-item-mode-id", "280:0" );
        config.addDefault( "prism.wands.default-block-mode-id", "17:1" );
        config.addDefault( "prism.wands.auto-equip", true );
        config.addDefault( "prism.wands.allow-user-override", true );
        final List<String> ignoreActionsForInspect = new ArrayList<String>();
        ignoreActionsForInspect.add( "player-chat" );
        ignoreActionsForInspect.add( "player-command" );
        ignoreActionsForInspect.add( "player-join" );
        ignoreActionsForInspect.add( "player-quit" );
        config.addDefault( "prism.wands.inspect.ignore-actions", ignoreActionsForInspect );

        // Queries
        config.addDefault( "prism.queries.default-radius", 5 );
        config.addDefault( "prism.queries.default-time-since", "3d" );
        config.addDefault( "prism.queries.max-lookup-radius", 100 );
        config.addDefault( "prism.queries.max-applier-radius", 100 );
        config.addDefault( "prism.queries.never-use-defaults", false );
        config.addDefault( "prism.queries.lookup-max-results", 1000 );
        config.addDefault( "prism.queries.default-results-per-page", 5 );
        config.addDefault( "prism.queries.lookup-auto-group", true );

        // Messenger
        config.addDefault( "prism.messenger.always-show-extended", false );

        // Near
        config.addDefault( "prism.near.default-radius", 5 );
        config.addDefault( "prism.near.max-results", 100 );

        // Drain
        config.addDefault( "prism.drain.max-radius", 10 );
        config.addDefault( "prism.drain.default-radius", 5 );

        // Ex
        config.addDefault( "prism.ex.max-radius", 100 );
        config.addDefault( "prism.ex.default-radius", 10 );

        // Ignore
        config.addDefault( "prism.ignore.enable-perm-nodes", false );
        config.addDefault( "prism.ignore.players-in-creative", false );
        config.addDefault( "prism.ignore.players", new ArrayList<String>() );
        config.addDefault( "prism.ignore.worlds", new ArrayList<String>() );

        // Purge
        final List<String> purgeRules = new ArrayList<String>();
        purgeRules.add( "before:8w" );
        purgeRules.add( "a:water-flow before:4w" );
        config.addDefault( "prism.db-records-purge-rules", purgeRules );
        config.addDefault( "prism.purge.batch-tick-delay", 60 );
        config.addDefault( "prism.purge.records-per-batch", 500000 );

        // Appliers
        config.addDefault( "prism.appliers.allow-rollback-items-removed-from-container", true );
        config.addDefault( "prism.appliers.notify-nearby.enabled", true );
        config.addDefault( "prism.appliers.notify-nearby.additional-radius", 20 );
        config.addDefault( "prism.appliers.remove-fire-on-burn-rollback", true );
        config.addDefault( "prism.appliers.remove-drops-on-explode-rollback", true );

        // Illegal Entity Rollbacks
        final List<String> illegalEntities = new ArrayList<String>();
        illegalEntities.add( "creeper" );
        config.addDefault( "prism.appliers.never-spawn-entity", illegalEntities );

        // Illegal Block Rollbacks
        final List<Integer> illegalBlocks = new ArrayList<Integer>();
        illegalBlocks.add( 10 );
        illegalBlocks.add( 11 );
        illegalBlocks.add( 46 );
        illegalBlocks.add( 51 );
        config.addDefault( "prism.appliers.never-place-block", illegalBlocks );

        // Tracking
        config.addDefault( "prism.tracking.block-break", true );
        config.addDefault( "prism.tracking.block-burn", true );
        config.addDefault( "prism.tracking.block-dispense", true );
        config.addDefault( "prism.tracking.block-fade", true );
        config.addDefault( "prism.tracking.block-fall", true );
        config.addDefault( "prism.tracking.block-form", true );
        config.addDefault( "prism.tracking.block-place", true );
        config.addDefault( "prism.tracking.block-shift", true );
        config.addDefault( "prism.tracking.block-spread", true );
        config.addDefault( "prism.tracking.block-use", true );
        config.addDefault( "prism.tracking.bucket-fill", true );
        config.addDefault( "prism.tracking.bonemeal-use", true );
        config.addDefault( "prism.tracking.container-access", true );
        config.addDefault( "prism.tracking.cake-eat", true );
        config.addDefault( "prism.tracking.craft-item", false );
        config.addDefault( "prism.tracking.creeper-explode", true );
        config.addDefault( "prism.tracking.crop-trample", true );
        config.addDefault( "prism.tracking.dragon-eat", true );
        config.addDefault( "prism.tracking.enchant-item", false );
        config.addDefault( "prism.tracking.enderman-pickup", true );
        config.addDefault( "prism.tracking.enderman-place", true );
        config.addDefault( "prism.tracking.entity-break", true );
        config.addDefault( "prism.tracking.entity-dye", false );
        config.addDefault( "prism.tracking.entity-explode", true );
        config.addDefault( "prism.tracking.entity-follow", true );
        config.addDefault( "prism.tracking.entity-form", true );
        config.addDefault( "prism.tracking.entity-kill", true );
        config.addDefault( "prism.tracking.entity-leash", true );
        config.addDefault( "prism.tracking.entity-shear", true );
        config.addDefault( "prism.tracking.entity-spawn", true );
        config.addDefault( "prism.tracking.entity-unleash", true );
        config.addDefault( "prism.tracking.fireball", true );
        config.addDefault( "prism.tracking.fire-spread", false );
        config.addDefault( "prism.tracking.firework-launch", true );
        config.addDefault( "prism.tracking.hangingitem-break", true );
        config.addDefault( "prism.tracking.hangingitem-place", true );
        config.addDefault( "prism.tracking.item-drop", true );
        config.addDefault( "prism.tracking.item-insert", true );
        config.addDefault( "prism.tracking.item-pickup", true );
        config.addDefault( "prism.tracking.item-remove", true );
        config.addDefault( "prism.tracking.item-rotate", true );
        config.addDefault( "prism.tracking.lava-break", true );
        config.addDefault( "prism.tracking.lava-bucket", true );
        config.addDefault( "prism.tracking.lava-flow", false );
        config.addDefault( "prism.tracking.lava-ignite", true );
        config.addDefault( "prism.tracking.leaf-decay", true );
        config.addDefault( "prism.tracking.lighter", true );
        config.addDefault( "prism.tracking.lightning", true );
        config.addDefault( "prism.tracking.mushroom-grow", true );
        config.addDefault( "prism.tracking.player-chat", false );
        config.addDefault( "prism.tracking.player-command", false );
        config.addDefault( "prism.tracking.player-death", true );
        config.addDefault( "prism.tracking.player-join", false );
        config.addDefault( "prism.tracking.player-kill", true );
        config.addDefault( "prism.tracking.player-quit", false );
        config.addDefault( "prism.tracking.player-teleport", false );
        config.addDefault( "prism.tracking.potion-splash", true );
        config.addDefault( "prism.tracking.sheep-eat", true );
        config.addDefault( "prism.tracking.sign-change", true );
        config.addDefault( "prism.tracking.spawnegg-use", true );
        config.addDefault( "prism.tracking.tnt-explode", true );
        config.addDefault( "prism.tracking.tnt-prime", true );
        config.addDefault( "prism.tracking.tree-grow", true );
        config.addDefault( "prism.tracking.vehicle-break", true );
        config.addDefault( "prism.tracking.vehicle-enter", true );
        config.addDefault( "prism.tracking.vehicle-exit", true );
        config.addDefault( "prism.tracking.vehicle-place", true );
        config.addDefault( "prism.tracking.water-break", true );
        config.addDefault( "prism.tracking.water-bucket", true );
        config.addDefault( "prism.tracking.water-flow", false );
        config.addDefault( "prism.tracking.world-edit", false );
        config.addDefault( "prism.tracking.xp-pickup", false );

        // Tracker configs
        config.addDefault( "prism.track-player-ip-on-join", false );
        config.addDefault( "prism.track-hopper-item-events", false );

        final List<String> doNotTrackCommand = new ArrayList<String>();
        doNotTrackCommand.add( "vanish" );
        doNotTrackCommand.add( "v" );
        doNotTrackCommand.add( "login" );
        doNotTrackCommand.add( "changepassword" );
        doNotTrackCommand.add( "register" );
        doNotTrackCommand.add( "unregister" );
        config.addDefault( "prism.do-not-track.commands", doNotTrackCommand );

        config.addDefault( "prism.tracking.api.enabled", true );
        final List<String> allowedApiPlugins = new ArrayList<String>();
        allowedApiPlugins.add( "DarkMythos" );
        allowedApiPlugins.add( "PrismApiDemo" );
        config.addDefault( "prism.tracking.api.allowed-plugins", allowedApiPlugins );

        // Ore Alerts
        config.addDefault( "prism.alerts.alert-staff-to-applied-process", true );
        config.addDefault( "prism.alerts.alert-player-about-self", true );
        config.addDefault( "prism.alerts.ores.enabled", true );
        config.addDefault( "prism.alerts.ores.log-to-console", true );
        config.addDefault( "prism.alerts.ores.log-commands", Arrays.asList("examplecommand <alert>") );
        // Ore blocks
        final HashMap<String, String> oreBlocks = new HashMap<String, String>();
        oreBlocks.put( "14", "&6" ); // iron
        oreBlocks.put( "15", "&7" ); // gold
        oreBlocks.put( "21", "&9" ); // lapis
        oreBlocks.put( "56", "&b" ); // diamond
        oreBlocks.put( "129", "&a" ); // emerald ore
        config.addDefault( "prism.alerts.ores.blocks", oreBlocks );

        // Illegal Command Alerts
        config.addDefault( "prism.alerts.illegal-commands.enabled", false );
        config.addDefault( "prism.alerts.illegal-commands.log-to-console", true );
        config.addDefault( "prism.alerts.illegal-commands.log-commands", Arrays.asList("examplecommand <alert>") );
        final List<String> illegal_commands = new ArrayList<String>();
        illegal_commands.add( "op" );
        illegal_commands.add( "deop" );
        illegal_commands.add( "stop" );
        illegal_commands.add( "reload" );
        illegal_commands.add( "bukkit:op" );
        illegal_commands.add( "bukkit:deop" );
        illegal_commands.add( "bukkit:stop" );
        illegal_commands.add( "bukkit:reload" );
        illegal_commands.add( "minecraft:op" );
        illegal_commands.add( "minecraft:deop" );
        illegal_commands.add( "minecraft:stop" );
        illegal_commands.add( "minecraft:reload" );
        config.addDefault( "prism.alerts.illegal-commands.commands", illegal_commands );

        // Use Alerts
        config.addDefault( "prism.alerts.uses.enabled", true );
        config.addDefault( "prism.alerts.uses.log-to-console", true );
        config.addDefault( "prism.alerts.uses.log-commands", Arrays.asList("examplecommand <alert>") );
        config.addDefault( "prism.alerts.uses.lighter", true );
        config.addDefault( "prism.alerts.uses.lava", true );

        List<String> monitorItems = new ArrayList<String>();
        monitorItems.add( "7" );
        monitorItems.add( "29" );
        monitorItems.add( "46" );
        monitorItems.add( "10" );
        monitorItems.add( "11" );
        config.addDefault( "prism.alerts.uses.item-placement", monitorItems );

        monitorItems = new ArrayList<String>();
        config.addDefault( "prism.alerts.uses.item-break", monitorItems );

        // Misc Alerts
        config.addDefault( "prism.alerts.vanilla-xray.enabled", true );

        // Internal
        config.addDefault( "prism.queue-empty-tick-delay", 3 );

        // Copy defaults
        config.options().copyDefaults( true );

        // save the defaults/config
        plugin.saveConfig();

        return config;

    }
}
