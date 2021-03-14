package me.botsko.prism.actionlibs;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.actions.*;
import me.botsko.prism.api.actions.ActionType;
import me.botsko.prism.exceptions.InvalidActionException;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.plugin.Plugin;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;

import static me.botsko.prism.api.actions.ActionType.*;

public class ActionRegistry {

    public static final HashMap<ActionType, Integer> prismActions = new HashMap<>();
    private final TreeMap<ActionType, ActionImpl> registeredActions = new TreeMap<>();

    public ActionRegistry() {
        registerPrismDefaultActions();
    }

    /**
     * Register a new action type for event recording, lookups, etc.
     *
     * @param action action.
     */
    private void registerAction(ActionImpl action) {
        registeredActions.put(action.getActionType(), action);
    }

    /**
     * Register a new action type for event recording, lookups, etc.  Actions must have a name with
     * 2 hyphens.  And the plugin must be on the allowed list of plugins.
     *
     * @param actionType type
     * @throws InvalidActionException if action not allowed.
     */
    @SuppressWarnings("unused")
    public void registerCustomAction(Plugin apiPlugin, ActionImpl actionType) throws InvalidActionException {
        final List<String> allowedPlugins = Prism.getInstance().config.trackingConfig.allowedPlugins;
        if (!allowedPlugins.contains(apiPlugin.getName())) {
            throw new InvalidActionException("Registering action type not allowed. Plugin '" + apiPlugin.getName()
                    + "' is not in list of allowed plugins.");
        }
        if (TypeUtils.subStrOccurences(actionType.getName(), "-") != 2) {
            throw new InvalidActionException("Invalid action type. Custom actions must contain two hyphens.");
        }
        Prism.getInstance().getPrismDataSource().addActionName(actionType.getActionType());
        registeredActions.put(actionType.getActionType(), actionType);
    }

    public TreeMap<ActionType, ActionImpl> getRegisteredAction() {
        return registeredActions;
    }

    /**
     * Get the Action for the type.
     *
     * @param name String
     * @return Action
     * @deprecated use {@link this#getAction(ActionType)}
     */
    @Deprecated
    public ActionImpl getAction(String name) {
        return getAction(ActionType.getByName(name));
    }

    /**
     * Get the Action for the type.
     * @param name ActionType
     * @return Action
     */
    public ActionImpl getAction(ActionType name) {
        return registeredActions.get(name);
    }

    /**
     * Retrieve a list of actions based on a search criteria.
     *
     * @param name to search by
     * @return {@code List<ActionType>}
     */
    public ArrayList<ActionImpl> getActionsByShortName(String name) {
        final ArrayList<ActionImpl> actions = new ArrayList<>();
        List<ActionType> types = ActionType.getByShortName(name);
        for (ActionType type : types) {
            actions.add(registeredActions.get(type));
        }
        return actions;
    }

    public ArrayList<ActionImpl> getActionsByFamilyName(String name) {
        final ArrayList<ActionImpl> actions = new ArrayList<>();
        List<ActionType> types = ActionType.getByFamilyName(name);
        for (ActionType type : types) {
            actions.add(registeredActions.get(type));
        }
        return actions;
    }
    /**
     * List all.
     *
     * @return list
     */
    public String[] listAll() {
        final String[] names = new String[registeredActions.size()];
        int i = 0;
        for (final Entry<ActionType, ActionImpl> entry : registeredActions.entrySet()) {
            names[i] = entry.getKey().toString();
            i++;
        }
        return names;
    }

    /**
     * List that allow rollback.
     *
     * @return list
     */
    @SuppressWarnings("unused")
    public ArrayList<String> listActionsThatAllowRollback() {
        final ArrayList<String> names = new ArrayList<>();
        for (final Entry<ActionType, ActionImpl> entry : registeredActions.entrySet()) {
            if (entry.getValue().canRollback()) {
                names.add(entry.getKey().toString());
            }
        }
        return names;
    }

    /**
     * List of actions that allow restore.
     *
     * @return list
     */
    @SuppressWarnings("unused")
    public ArrayList<String> listActionsThatAllowRestore() {
        final ArrayList<String> names = new ArrayList<>();
        for (final Entry<ActionType, ActionImpl> entry : registeredActions.entrySet()) {
            if (entry.getValue().canRestore()) {
                names.add(entry.getKey().toString());
            }
        }
        return names;
    }

    @SuppressWarnings("DuplicatedCode")
    private void registerPrismDefaultActions() {

        registerAction(new ActionImpl(BLOCK_BREAK, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl(BLOCK_BURN, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("burned")));
        registerAction(new ActionImpl(BLOCK_DISPENSE, false, false, false,
                ItemStackAction.class, Il8nHelper.getRawMessage("dispensed")));
        registerAction(new ActionImpl(BLOCK_FADE, false, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("faded")));
        registerAction(new ActionImpl(BLOCK_FALL, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("fell")));
        registerAction(new ActionImpl(BLOCK_FORM, false, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("formed")));
        registerAction(new ActionImpl(BLOCK_PLACE, true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("placed")));
        registerAction(new ActionImpl(BLOCK_SHIFT, true, false, false,
                BlockShiftAction.class, Il8nHelper.getRawMessage("moved")));
        registerAction(new ActionImpl(BLOCK_SPREAD, true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("grew")));
        registerAction(new ActionImpl(BLOCK_USE, false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("used")));
        registerAction(new ActionImpl(BLOCK_USE, false, false, false,
                UseAction.class, Il8nHelper.getRawMessage("used")));
        registerAction(new ActionImpl(BUCKET_FILL, false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("filled")));
        registerAction(new ActionImpl(CAKE_EAT, false, false, false,
                UseAction.class, Il8nHelper.getRawMessage("ate")));
        registerAction(new ActionImpl(CONTAINER_ACCESS, false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("accessed")));
        registerAction(new ActionImpl(CRAFT_ITEM, false, false, false,
                ItemStackAction.class, Il8nHelper.getRawMessage("crafted")));
        registerAction(new ActionImpl(CREEPER_EXPLODE, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("blew-up")));
        registerAction(new ActionImpl(CROP_TRAMPLE, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("trampled")));
        registerAction(new ActionImpl(DRAGON_EAT, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("ate")));
        registerAction(new ActionImpl(ENCHANT_ITEM, false, false, false,
                ItemStackAction.class, Il8nHelper.getRawMessage("enchanted")));
        registerAction(new ActionImpl(ENDERMAN_PICKUP, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("picked-up")));
        registerAction(new ActionImpl(ENDERMAN_PLACE, true, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("placed")));
        registerAction(new ActionImpl(ENTITY_BREAK, true, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl(ENTITY_DYE, false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("dyed")));
        registerAction(new ActionImpl(ENTITY_EXPLODE, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("blew-up")));
        registerAction(new ActionImpl(ENTITY_FOLLOW, false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("lured")));
        registerAction(new ActionImpl(ENTITY_FORM, true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("formed")));
        registerAction(new ActionImpl(ENTITY_KILL, false, true, false,
                EntityAction.class, Il8nHelper.getRawMessage("killed")));
        registerAction(new ActionImpl(ENTITY_LEASH, true, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("leashed")));
        registerAction(new ActionImpl(ENTITY_SHEAR, false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("sheared")));
        registerAction(new ActionImpl(ENTITY_SPAWN, false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("spawned")));
        registerAction(new ActionImpl(ENTITY_UNLEASH, false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("unleashed")));
        registerAction(new ActionImpl(FIREBALL, false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("ignited")));
        registerAction(new ActionImpl(FIRE_SPREAD, true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("spread")));
        registerAction(new ActionImpl(FIREWORK_LAUNCH, false, false, false,
                ItemStackAction.class, Il8nHelper.getRawMessage("launched")));
        registerAction(new ActionImpl(HANGINGITEM_BREAK, false, true, true,
                HangingItemAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl(HANGINGITEM_PLACE, true, true, true,
                HangingItemAction.class, Il8nHelper.getRawMessage("hung")));
        registerAction(new ActionImpl(ITEM_DROP, false, true, true,
                ItemStackAction.class, Il8nHelper.getRawMessage("dropped")));
        registerAction(new ActionImpl(ITEM_INSERT, false, true, true,
                ItemStackAction.class, Il8nHelper.getRawMessage("inserted")));
        registerAction(new ActionImpl(ITEM_PICKUP, false, true, true,
                ItemStackAction.class, Il8nHelper.getRawMessage("picked-up")));
        registerAction(new ActionImpl(ITEM_REMOVE, false, true, true,
                ItemStackAction.class, Il8nHelper.getRawMessage("removed")));
        registerAction(new ActionImpl(ITEM_BREAK, false, false, false,
                ItemStackAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl(ITEM_ROTATE, false, false, false,
                UseAction.class, Il8nHelper.getRawMessage("turned-item")));
        registerAction(new ActionImpl(LAVA_BREAK, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl(LAVA_BUCKET, true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("poured")));
        registerAction(new ActionImpl(LAVA_FLOW, true, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("flowed-into")));
        registerAction(new ActionImpl(LAVA_IGNITE, false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("ignited")));
        registerAction(new ActionImpl(LEAF_DECAY, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("decayed")));
        registerAction(new ActionImpl(LIGHTER, false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("set-fire")));
        registerAction(new ActionImpl(LIGHTNING, false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("ignited")));
        registerAction(new ActionImpl(MUSHROOM_GROW, true, true, true,
                GrowAction.class, Il8nHelper.getRawMessage("grew")));
        registerAction(new ActionImpl(PLAYER_CHAT, false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("said")));
        registerAction(new ActionImpl(PLAYER_COMMAND, false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("command-run")));
        registerAction(new ActionImpl(PLAYER_DEATH, false, false, false,
                PlayerDeathAction.class, Il8nHelper.getRawMessage("died")));
        registerAction(new ActionImpl(PLAYER_JOIN, false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("joined")));
        registerAction(new ActionImpl(PLAYER_KILL, false, true, false,
                EntityAction.class, Il8nHelper.getRawMessage("killed")));
        registerAction(new ActionImpl(PLAYER_QUIT, false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("quit")));
        registerAction(new ActionImpl(PLAYER_GAMEMODECHANGE, false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("changed_game_mode")));
        registerAction(new ActionImpl(PLAYER_TELEPORT, false, false, false,
                EntityTravelAction.class, Il8nHelper.getRawMessage("teleported")));
        registerAction(new ActionImpl(POTION_SPLASH, false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("potion-throw")));
        registerAction(new ActionImpl(PRISM_DRAIN, false, true, true,
                PrismRollbackAction.class, Il8nHelper.getRawMessage("drained")));
        registerAction(new ActionImpl(PRISM_EXTINGUISH, false, true, true,
                PrismRollbackAction.class, Il8nHelper.getRawMessage("extinguished")));
        registerAction(new ActionImpl(PRISM_PROCESS, false, false, false,
                PrismProcessAction.class, Il8nHelper.getRawMessage("ran-process")));
        registerAction(new ActionImpl(PRISM_ROLLBACK, true, false, false,
                PrismRollbackAction.class, Il8nHelper.getRawMessage("rolled-back")));
        registerAction(new ActionImpl(SHEEP_EAT, false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("ate")));
        registerAction(new ActionImpl(SIGN_CHANGE, false, false, true,
                SignAction.class, Il8nHelper.getRawMessage("wrote")));
        registerAction(new ActionImpl(SPAWNEGG_USE, false, false, false,
                UseAction.class, Il8nHelper.getRawMessage("used")));
        registerAction(new ActionImpl(TNT_EXPLODE, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("blew-up")));
        registerAction(new ActionImpl(BED_EXPLODE, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("blew-up")));
        registerAction(new ActionImpl(TNT_PRIME, false, false, false,
                UseAction.class, Il8nHelper.getRawMessage("primed")));
        registerAction(new ActionImpl(TREE_GROW, true, true, true,
                GrowAction.class, Il8nHelper.getRawMessage("grew")));
        registerAction(new ActionImpl(VEHICLE_BREAK, false, true, false,
                VehicleAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl(VEHICLE_ENTER, false, false, false,
                VehicleAction.class, Il8nHelper.getRawMessage("entered")));
        registerAction(new ActionImpl(VEHICLE_ENTER, false, false, false,
                VehicleAction.class, Il8nHelper.getRawMessage("exited")));
        registerAction(new ActionImpl(VEHICLE_PLACE, true, false, false,
                VehicleAction.class, Il8nHelper.getRawMessage("placed")));
        registerAction(new ActionImpl(WATER_BREAK, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl(WATER_BUCKET, true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("poured")));
        registerAction(new ActionImpl(WATER_FLOW, true, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("flowed-into")));
        registerAction(new ActionImpl(WORLD_EDIT, true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("edited")));
        registerAction(new ActionImpl(XP_PICKUP, false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("picked-up")));
        if (Prism.isPaper) {
            registerAction(new ActionImpl(TARGET_HIT, false, false, false,
                    BlockAction.class, Il8nHelper.getRawMessage("hit_and_triggered")));
            registerAction(new ActionImpl(PLAYER_TRADE, false, false, false,
                    EntityAction.class, Il8nHelper.getRawMessage("traded_with")));
            registerAction(new ActionImpl(ITEM_RECEIVE, false, true, true,
                    ItemStackAction.class, Il8nHelper.getRawMessage("received")));
        }
    }
}