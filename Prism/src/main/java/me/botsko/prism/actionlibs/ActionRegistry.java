package me.botsko.prism.actionlibs;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.BlockChangeAction;
import me.botsko.prism.actions.BlockShiftAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.EntityTravelAction;
import me.botsko.prism.actions.GrowAction;
import me.botsko.prism.actions.HangingItemAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.PlayerAction;
import me.botsko.prism.actions.PlayerDeathAction;
import me.botsko.prism.actions.PrismProcessAction;
import me.botsko.prism.actions.PrismRollbackAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.actions.UseAction;
import me.botsko.prism.actions.VehicleAction;
import me.botsko.prism.api.actions.ActionType;
import me.botsko.prism.exceptions.InvalidActionException;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.plugin.Plugin;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;

import static me.botsko.prism.api.actions.ActionType.*;

public class ActionRegistry {

    private final TreeMap<String, ActionImpl> registeredActions = new TreeMap<>();

    public ActionRegistry() {
        registerPrismDefaultActions();
    }

    /**
     * Register a new action type for event recording, lookups, etc.
     *
     * @param actionType type
     */
    private void registerAction(ActionImpl actionType) {
        registeredActions.put(actionType.getName(), actionType);
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
        final List<String> allowedPlugins = Prism.config.getStringList("prism.tracking.api.allowed-plugins");
        if (!allowedPlugins.contains(apiPlugin.getName())) {
            throw new InvalidActionException("Registering action type not allowed. Plugin '" + apiPlugin.getName()
                    + "' is not in list of allowed plugins.");
        }
        if (TypeUtils.subStrOccurences(actionType.getName(), "-") != 2) {
            throw new InvalidActionException("Invalid action type. Custom actions must contain two hyphens.");
        }
        Prism.getInstance().getPrismDataSource().addActionName(actionType.getName());
        registeredActions.put(actionType.getName(), actionType);
    }

    public TreeMap<String, ActionImpl> getRegisteredAction() {
        return registeredActions;
    }

    public ActionImpl getAction(String name) {
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
        for (final Entry<String, ActionImpl> entry : registeredActions.entrySet()) {
            // Match either the name or the short name
            if (entry.getValue().getFamilyName().equals(name) || entry.getValue().getShortName().equals(name)
                    || entry.getValue().getName().equals(name)) {
                actions.add(entry.getValue());
            }
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
        for (final Entry<String, ActionImpl> entry : registeredActions.entrySet()) {
            names[i] = entry.getKey();
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
        for (final Entry<String, ActionImpl> entry : registeredActions.entrySet()) {
            if (entry.getValue().canRollback()) {
                names.add(entry.getKey());
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
        for (final Entry<String, ActionImpl> entry : registeredActions.entrySet()) {
            if (entry.getValue().canRestore()) {
                names.add(entry.getKey());
            }
        }
        return names;
    }

    private void registerPrismDefaultActions() {

        registerAction(new ActionImpl(ActionType.BLOCK_BREAK, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl(ActionType.BLOCK_BURN, false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("burned")));
        registerAction(new ActionImpl(ActionType.BLOCK_DISPENSE, false, false, false,
                ItemStackAction.class, Il8nHelper.getRawMessage("dispensed")));
        registerAction(new ActionImpl(ActionType.BLOCK_FADE, false, true, true,
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
        registerAction(new ActionImpl("crop-trample", false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("trampled")));
        registerAction(new ActionImpl("dragon-eat", false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("ate")));
        registerAction(new ActionImpl("enchant-item", false, false, false,
                ItemStackAction.class, Il8nHelper.getRawMessage("enchanted")));
        registerAction(new ActionImpl("enderman-pickup", false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("picked-up")));
        registerAction(new ActionImpl("enderman-place", true, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("placed")));
        registerAction(new ActionImpl("entity-break", true, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl("entity-dye", false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("dyed")));
        registerAction(new ActionImpl("entity-explode", false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("blew-up")));
        registerAction(new ActionImpl("entity-follow", false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("lured")));
        registerAction(new ActionImpl("entity-form", true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("formed")));
        registerAction(new ActionImpl("entity-kill", false, true, false,
                EntityAction.class, Il8nHelper.getRawMessage("killed")));
        registerAction(new ActionImpl("entity-leash", true, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("leashed")));
        registerAction(new ActionImpl("entity-shear", false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("sheared")));
        registerAction(new ActionImpl("entity-spawn", false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("spawned")));
        registerAction(new ActionImpl("entity-unleash", false, false, false,
                EntityAction.class, Il8nHelper.getRawMessage("unleashed")));
        registerAction(new ActionImpl("fireball", false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("ignited")));
        registerAction(new ActionImpl("fire-spread", true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("spread")));
        registerAction(new ActionImpl("firework-launch", false, false, false,
                ItemStackAction.class, Il8nHelper.getRawMessage("launched")));
        registerAction(new ActionImpl("hangingitem-break", false, true, true,
                HangingItemAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl("hangingitem-place", true, true, true,
                HangingItemAction.class, Il8nHelper.getRawMessage("hung")));
        registerAction(new ActionImpl("item-drop", false, true, true,
                ItemStackAction.class, Il8nHelper.getRawMessage("dropped")));
        registerAction(new ActionImpl("item-insert", false, true, true,
                ItemStackAction.class, Il8nHelper.getRawMessage("inserted")));
        registerAction(new ActionImpl("item-pickup", false, true, true,
                ItemStackAction.class, Il8nHelper.getRawMessage("picked-up")));
        registerAction(new ActionImpl("item-remove", false, true, true,
                ItemStackAction.class, Il8nHelper.getRawMessage("removed")));
        registerAction(new ActionImpl("item-break", false, false, false,
                ItemStackAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl("item-rotate", false, false, false,
                UseAction.class, Il8nHelper.getRawMessage("turned-item")));
        registerAction(new ActionImpl("lava-break", false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl("lava-bucket", true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("poured")));
        registerAction(new ActionImpl("lava-flow", true, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("flowed-into")));
        registerAction(new ActionImpl("lava-ignite", false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("ignited")));
        registerAction(new ActionImpl("leaf-decay", false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("decayed")));
        registerAction(new ActionImpl("lighter", false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("set-fire")));
        registerAction(new ActionImpl("lightning", false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("ignited")));
        registerAction(new ActionImpl("mushroom-grow", true, true, true,
                GrowAction.class, Il8nHelper.getRawMessage("grew")));
        registerAction(new ActionImpl("player-chat", false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("said")));
        registerAction(new ActionImpl("player-command", false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("command-run")));
        registerAction(new ActionImpl("player-death", false, false, false,
                PlayerDeathAction.class, Il8nHelper.getRawMessage("died")));
        registerAction(new ActionImpl("player-join", false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("joined")));
        registerAction(new ActionImpl("player-kill", false, true, false,
                EntityAction.class, Il8nHelper.getRawMessage("killed")));
        registerAction(new ActionImpl("player-quit", false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("quit")));
        registerAction(new ActionImpl("player-gamemodechange", false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("changed_game_mode")));
        registerAction(new ActionImpl("player-teleport", false, false, false,
                EntityTravelAction.class, Il8nHelper.getRawMessage("teleported")));
        registerAction(new ActionImpl("potion-splash", false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("potion-throw")));
        registerAction(new ActionImpl("prism-drain", false, true, true,
                PrismRollbackAction.class, Il8nHelper.getRawMessage("drained")));
        registerAction(new ActionImpl("prism-extinguish", false, true, true,
                PrismRollbackAction.class, Il8nHelper.getRawMessage("extinguished")));
        registerAction(new ActionImpl("prism-process", false, false, false,
                PrismProcessAction.class, Il8nHelper.getRawMessage("ran-process")));
        registerAction(new ActionImpl("prism-rollback", true, false, false,
                PrismRollbackAction.class, Il8nHelper.getRawMessage("rolled-back")));
        registerAction(new ActionImpl("sheep-eat", false, false, false,
                BlockAction.class, Il8nHelper.getRawMessage("ate")));
        registerAction(new ActionImpl("sign-change", false, false, true,
                SignAction.class, Il8nHelper.getRawMessage("wrote")));
        registerAction(new ActionImpl("spawnegg-use", false, false, false,
                UseAction.class, Il8nHelper.getRawMessage("used")));
        registerAction(new ActionImpl("tnt-explode", false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("blew-up")));
        registerAction(new ActionImpl("bed-explode", false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("blew-up")));
        registerAction(new ActionImpl("tnt-prime", false, false, false,
                UseAction.class, Il8nHelper.getRawMessage("primed")));
        registerAction(new ActionImpl("tree-grow", true, true, true,
                GrowAction.class, Il8nHelper.getRawMessage("grew")));
        registerAction(new ActionImpl("vehicle-break", false, true, false,
                VehicleAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl("vehicle-enter", false, false, false,
                VehicleAction.class, Il8nHelper.getRawMessage("entered")));
        registerAction(new ActionImpl("vehicle-exit", false, false, false,
                VehicleAction.class, Il8nHelper.getRawMessage("exited")));
        registerAction(new ActionImpl("vehicle-place", true, false, false,
                VehicleAction.class, Il8nHelper.getRawMessage("placed")));
        registerAction(new ActionImpl("water-break", false, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("broke")));
        registerAction(new ActionImpl("water-bucket", true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("poured")));
        registerAction(new ActionImpl("water-flow", true, true, true,
                BlockAction.class, Il8nHelper.getRawMessage("flowed-into")));
        registerAction(new ActionImpl("world-edit", true, true, true,
                BlockChangeAction.class, Il8nHelper.getRawMessage("edited")));
        registerAction(new ActionImpl("xp-pickup", false, false, false,
                PlayerAction.class, Il8nHelper.getRawMessage("picked-up")));
        if (Prism.isPaper) {
            registerAction(new ActionImpl("target-hit", false, false, false,
                    BlockAction.class, Il8nHelper.getRawMessage("hit_and_triggered")));
            registerAction(new ActionImpl("player-trade", false, false, false,
                    EntityAction.class, Il8nHelper.getRawMessage("traded_with")));
            registerAction(new ActionImpl("item-receive", false, true, true,
                    ItemStackAction.class, Il8nHelper.getRawMessage("received")));
        }
    }
}