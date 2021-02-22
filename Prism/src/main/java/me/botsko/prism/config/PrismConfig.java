package me.botsko.prism.config;

import org.bukkit.Material;
import org.bukkit.entity.EntityType;
import org.spongepowered.configurate.objectmapping.ConfigSerializable;
import org.spongepowered.configurate.objectmapping.meta.Setting;

import java.util.*;

/**
 * Created for Prism.
 *
 * @author Narimm on 22/02/2021
 * @since 2.1.8
 */
@ConfigSerializable
public class PrismConfig {

    @Setting("debug")
    public boolean debug = false;

    @Setting("preleoad-materials")
    public boolean preloadMaterials = true;

    @Setting("paste")
    public PasteConfig pasteConfig = new PasteConfig();

    @Setting("wand")
    public WandConfig wandConfig = new WandConfig();

    @Setting("query")
    public ParameterConfig parameterConfig = new ParameterConfig();

    @Setting("near")
    public CommandDefault nearCommandCondig = new CommandDefault(5,100,100);

    @Setting("drain")
    public CommandDefault drainCommandConfig = new CommandDefault(5,100,10);

    @Setting("ex")
    public CommandDefault extinguishCommandConfig = new CommandDefault(5,100,10);

    @Setting("ignore")
    public IgnoreConfig ignoreConfig = new IgnoreConfig();

    @Setting("applier")
    public ApplierConfig applierConfig = new ApplierConfig();


    @ConfigSerializable
    public static class PasteConfig {

        @Setting("enable")
        public boolean enabled = false;

        @Setting("api-key")
        public String apiKey = "API key from http://paste.gg";

    }

    @ConfigSerializable
    public static class WandConfig {

        @Setting("default-mode")
        public WandMode default_mode = WandMode.HAND;

        @Setting("default-mode-item")
        public Material default_mode_item = Material.STICK;

        @Setting("default-mode-block")
        public Material default_mode_block = Material.SPRUCE_LOG;

        @Setting("auto-equip")
        public boolean autoEquip = true;

        @Setting("allow-user-override")
        public boolean allowUserOverride = true;

        @Setting("inspect-ignore-actions")
        public List<String> wandInspectIgnoreActions = Arrays.asList("player-chat","player-command",
                "player-join","player-quit");

    }

    public static class ParameterConfig {

        @Setting("default-radius")
        public int defaultRadius = 5;

        @Setting("default-time-since")
        public String defaultTimeSince = "2d";

        @Setting("max-lookup-radius")
        public int maxLookupRadius = 100;

        @Setting("max-applier-radius")
        public int maxApplierRadius = 100;

        @Setting("never-use-defaults")
        public boolean neverUseDefaults = false;

        @Setting("lookup-max-results")
        public int lookupMaxResults = 1000;

        @Setting("default-results-per-page")
        public int defaultResultsPerPage = 5;

        @Setting("lookup-auto-group")
        public boolean lookupAutoGroup = true;

        @Setting("always-show-extended")
        public boolean  alwaysShowExtended = false;

    }

    public static class CommandDefault {

        @Setting("default-radius")
        public int defaultRadius;

        @Setting("max-results")
        public int maxResults;

        @Setting("max-radius")
        public int maxRadius;

        protected CommandDefault(int defaultRadius, int maxResults, int maxRadius) {
            this.defaultRadius = defaultRadius;
            this.maxResults = maxResults;
            this.maxRadius = maxRadius;
        }
    }

    public static class IgnoreConfig {

        @Setting("enable-perm-nodes")
        public boolean enablePermNodes = false;

        @Setting("players-in-creative")
        public boolean creativePlayers = false;

        @Setting("players")
        public List<UUID> players = new ArrayList<>();

        @Setting("players_whitelist")
        public boolean isPlayerWhiteList = false;

        @Setting("worlds")
        public List<UUID> worlds = new ArrayList<>();

        @Setting("worlds_whitelist")
        public boolean isWorldWhiteList = false;

    }

    public static class PurgeConfig {


        @Setting("allow-rollback-items-removed-from-container")
        public boolean allowRollbackItemsRemovedFromContainer = true;        @Setting("rules")
        public List<String> rules = Arrays.asList("before:8w", "a:water-flow before:4w");

        @Setting("batch-tick-delay")
        public int batchTickDelay = 30;

        @Setting("records-per-batch")
        public int recordsPerBatch = 10000;

    }

    public static class ApplierConfig {


        @Setting("notify-nearby-enabled")
        public boolean notifyNearby = true;

        @Setting("notify-nearby-additional-radius")
        public int additionalNotifyRadius = 20;

        @Setting("remove-fire-on-burn-rollback")
        public boolean removeFireOnBurnRollback = true;

        @Setting("remove-drops-on-explode-rollback")
        public boolean removeDropsExplodeRollback = true;

        @Setting("never-spawn-entity")
        public EnumSet<EntityType> neverSpawn = EnumSet.of(EntityType.CREEPER);

        @Setting("never-place-block")
        public EnumSet<Material> neverPlace = EnumSet.of(Material.LAVA,Material.WATER);
        
    }
    
    public static class TrackingConfig {
        public Map<String,Boolean> trackers = new HashMap<>();

        public TrackingConfig() {
            trackers.put("block-break", true);
            trackers.put("block-burn", true);
            trackers.put("block-dispense", true);
            trackers.put("block-fade", true);
            trackers.put("block-fall", true);
            trackers.put("block-form", true);
            trackers.put("block-place", true);
            trackers.put("block-shift", true);
            trackers.put("block-spread", true);
            trackers.put("block-use", true);
            trackers.put("bucket-fill", true);
            trackers.put("bonemeal-use", true);
            trackers.put("container-access", true);
            trackers.put("cake-eat", true);
            trackers.put("craft-item", false);
            trackers.put("creeper-explode", true);
            trackers.put("crop-trample", true);
            trackers.put("dragon-eat", true);
            trackers.put("enchant-item", false);
            trackers.put("enderman-pickup", true);
            trackers.put("enderman-place", true);
            trackers.put("entity-break", true);
            trackers.put("entity-dye", false);
            trackers.put("entity-explode", true);
            trackers.put("entity-follow", true);
            trackers.put("entity-form", true);
            trackers.put("entity-kill", true);
            trackers.put("entity-leash", true);
            trackers.put("entity-shear", true);
            trackers.put("entity-spawn", true);
            trackers.put("entity-unleash", true);
            trackers.put("fireball", true);
            trackers.put("fire-spread", false);
            trackers.put("firework-launch", true);
            trackers.put("hangingitem-break", true);
            trackers.put("hangingitem-place", true);
            trackers.put("item-drop", true);
            trackers.put("item-insert", true);
            trackers.put("item-pickup", true);
            trackers.put("item-remove", true);
            trackers.put("item-break", false);
            trackers.put("item-rotate", true);
            trackers.put("lava-break", true);
            trackers.put("lava-bucket", true);
            trackers.put("lava-flow", false);
            trackers.put("lava-ignite", true);
            trackers.put("leaf-decay", true);
            trackers.put("lighter", true);
            trackers.put("lightning", true);
            trackers.put("mushroom-grow", true);
            trackers.put("player-chat", false);
            trackers.put("player-command", false);
            trackers.put("player-death", true);
            trackers.put("player-gamemodechange", false);
            trackers.put("player-join", false);
            trackers.put("player-kill", true);
            trackers.put("player-quit", false);
            trackers.put("player-teleport", false);
            trackers.put("player-trade", false);
            trackers.put("potion-splash", true);
            trackers.put("sheep-eat", true);
            trackers.put("sign-change", true);
            trackers.put("spawnegg-use", true);
            trackers.put("target-hit", false);
            trackers.put("tnt-explode", true);
            trackers.put("tnt-prime", true);
            trackers.put("tree-grow", true);
            trackers.put("vehicle-break", true);
            trackers.put("vehicle-enter", true);
            trackers.put("vehicle-exit", true);
            trackers.put("vehicle-place", true);
            trackers.put("water-break", true);
            trackers.put("water-bucket", true);
            trackers.put("water-flow", false);
            trackers.put("world-edit", false);
            trackers.put("xp-pickup", false);
        }
    }

    public enum WandMode {
        HAND,
        ITEM,
        BLOCK
    }
}
