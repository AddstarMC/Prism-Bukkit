package me.botsko.prism.actionlibs;

import me.botsko.prism.Il8n;
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
import me.botsko.prism.exceptions.InvalidActionException;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.plugin.Plugin;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;

public class ActionRegistry {

    private final TreeMap<String, ActionType> registeredActions = new TreeMap<>();

    public ActionRegistry() {
        registerPrismDefaultActions();
    }

    /**
     * Register a new action type for event recording, lookups, etc.
     *
     * @param actionType type
     */
    private void registerAction(ActionType actionType) {
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
    public void registerCustomAction(Plugin apiPlugin, ActionType actionType) throws InvalidActionException {
        final List<String> allowedPlugins = Prism.config.getStringList("prism.tracking.api.allowed-plugins");
        if (!allowedPlugins.contains(apiPlugin.getName())) {
            throw new InvalidActionException("Registering action type not allowed. Plugin '" + apiPlugin.getName()
                    + "' is not in list of allowed plugins.");
        }
        if (TypeUtils.subStrOccurences(actionType.getName(), "-") != 2) {
            throw new InvalidActionException("Invalid action type. Custom actions must contain two hyphens.");
        }
        Prism.getPrismDataSource().addActionName(actionType.getName());
        registeredActions.put(actionType.getName(), actionType);
    }

    public TreeMap<String, ActionType> getRegisteredAction() {
        return registeredActions;
    }

    public ActionType getAction(String name) {
        return registeredActions.get(name);
    }

    /**
     * Retrieve a list of actions based on a search criteria.
     *
     * @param name to search by
     * @return {@code List<ActionType>}
     */
    public ArrayList<ActionType> getActionsByShortName(String name) {
        final ArrayList<ActionType> actions = new ArrayList<>();
        for (final Entry<String, ActionType> entry : registeredActions.entrySet()) {
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
        for (final Entry<String, ActionType> entry : registeredActions.entrySet()) {
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
        for (final Entry<String, ActionType> entry : registeredActions.entrySet()) {
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
        for (final Entry<String, ActionType> entry : registeredActions.entrySet()) {
            if (entry.getValue().canRestore()) {
                names.add(entry.getKey());
            }
        }
        return names;
    }

    private void registerPrismDefaultActions() {

        registerAction(new ActionType("block-break", false, true, true,
                BlockAction.class, Il8n.getRawMessage("broke")));
        registerAction(new ActionType("block-burn", false, true, true,
                BlockAction.class, Il8n.getRawMessage("burned")));
        registerAction(new ActionType("block-dispense", false, false, false,
                ItemStackAction.class, Il8n.getRawMessage("dispensed")));
        registerAction(new ActionType("block-fade", false, true, true,
                BlockChangeAction.class, Il8n.getRawMessage("faded")));
        registerAction(new ActionType("block-fall", false, true, true,
                BlockAction.class, Il8n.getRawMessage("fell")));
        registerAction(new ActionType("block-form", false, true, true,
                BlockChangeAction.class, Il8n.getRawMessage("formed")));
        registerAction(new ActionType("block-place", true, true, true,
                BlockChangeAction.class, Il8n.getRawMessage("placed")));
        registerAction(new ActionType("block-shift", true, false, false,
                BlockShiftAction.class, Il8n.getRawMessage("moved")));
        registerAction(new ActionType("block-spread", true, true, true,
                BlockChangeAction.class, Il8n.getRawMessage("grew")));
        registerAction(new ActionType("block-use", false, false, false,
                BlockAction.class, Il8n.getRawMessage("used")));
        registerAction(new ActionType("bonemeal-use", false, false, false,
                UseAction.class, Il8n.getRawMessage("used")));
        registerAction(new ActionType("bucket-fill", false, false, false,
                PlayerAction.class, Il8n.getRawMessage("filled")));
        registerAction(new ActionType("cake-eat", false, false, false,
                UseAction.class, Il8n.getRawMessage("ate")));
        registerAction(new ActionType("container-access", false, false, false,
                BlockAction.class, Il8n.getRawMessage("accessed")));
        registerAction(new ActionType("craft-item", false, false, false,
                ItemStackAction.class, Il8n.getRawMessage("crafted")));
        registerAction(new ActionType("creeper-explode", false, true, true,
                BlockAction.class, Il8n.getRawMessage("blew up")));
        registerAction(new ActionType("crop-trample", false, true, true,
                BlockAction.class, Il8n.getRawMessage("trampled")));
        registerAction(new ActionType("dragon-eat", false, true, true,
                BlockAction.class, Il8n.getRawMessage("ate")));
        registerAction(new ActionType("enchant-item", false, false, false,
                ItemStackAction.class, Il8n.getRawMessage("enchanted")));
        registerAction(new ActionType("enderman-pickup", false, true, true,
                BlockAction.class, Il8n.getRawMessage("picked-up")));
        registerAction(new ActionType("enderman-place", true, true, true,
                BlockAction.class, Il8n.getRawMessage("placed")));
        registerAction(new ActionType("entity-break", true, true, true,
                BlockAction.class, Il8n.getRawMessage("broke")));
        registerAction(new ActionType("entity-dye", false, false, false,
                EntityAction.class, Il8n.getRawMessage("dyed")));
        registerAction(new ActionType("entity-explode", false, true, true,
                BlockAction.class, Il8n.getRawMessage("blew up")));
        registerAction(new ActionType("entity-follow", false, false, false,
                EntityAction.class, Il8n.getRawMessage("lured")));
        registerAction(new ActionType("entity-form", true, true, true,
                BlockChangeAction.class, Il8n.getRawMessage("formed")));
        registerAction(new ActionType("entity-kill", false, true, false,
                EntityAction.class, Il8n.getRawMessage("killed")));
        registerAction(new ActionType("entity-leash", true, false, false,
                EntityAction.class, Il8n.getRawMessage("leashed")));
        registerAction(new ActionType("entity-shear", false, false, false,
                EntityAction.class, Il8n.getRawMessage("sheared")));
        registerAction(new ActionType("entity-spawn", false, false, false,
                EntityAction.class, Il8n.getRawMessage("spawned")));
        registerAction(new ActionType("entity-unleash", false, false, false,
                EntityAction.class, Il8n.getRawMessage("unleashed")));
        registerAction(new ActionType("fireball", false, false, false,
                BlockAction.class, Il8n.getRawMessage("ignited")));
        registerAction(new ActionType("fire-spread", true, true, true,
                BlockChangeAction.class, Il8n.getRawMessage("spread")));
        registerAction(new ActionType("firework-launch", false, false, false,
                ItemStackAction.class, Il8n.getRawMessage("launched")));
        registerAction(new ActionType("hangingitem-break", false, true, true,
                HangingItemAction.class, Il8n.getRawMessage("broke")));
        registerAction(new ActionType("hangingitem-place", true, true, true,
                HangingItemAction.class, Il8n.getRawMessage("hung")));
        registerAction(new ActionType("item-drop", false, true, true,
                ItemStackAction.class, Il8n.getRawMessage("dropped")));
        registerAction(new ActionType("item-insert", false, true, true,
                ItemStackAction.class, Il8n.getRawMessage("inserted")));
        registerAction(new ActionType("item-pickup", false, true, true,
                ItemStackAction.class, Il8n.getRawMessage("picked-up")));
        registerAction(new ActionType("item-remove", false, true, true,
                ItemStackAction.class, Il8n.getRawMessage("removed")));
        registerAction(new ActionType("item-break",false,false,false,
                ItemStackAction.class,Il8n.getRawMessage("broke")));
        registerAction(new ActionType("item-rotate", false, false, false,
                UseAction.class, Il8n.getRawMessage("turned-item")));
        registerAction(new ActionType("lava-break", false, true, true,
                BlockAction.class, Il8n.getRawMessage("broke")));
        registerAction(new ActionType("lava-bucket", true, true, true,
                BlockChangeAction.class, Il8n.getRawMessage("poured")));
        registerAction(new ActionType("lava-flow", true, true, true,
                BlockAction.class, Il8n.getRawMessage("flowed-into")));
        registerAction(new ActionType("lava-ignite", false, false, false,
                BlockAction.class, Il8n.getRawMessage("ignited")));
        registerAction(new ActionType("leaf-decay", false, true, true,
                BlockAction.class, Il8n.getRawMessage("decayed")));
        registerAction(new ActionType("lighter", false, false, false,
                BlockAction.class, Il8n.getRawMessage("set-fire")));
        registerAction(new ActionType("lightning", false, false, false,
                BlockAction.class, Il8n.getRawMessage("ignited")));
        registerAction(new ActionType("mushroom-grow", true, true, true,
                GrowAction.class, Il8n.getRawMessage("grew")));
        registerAction(new ActionType("player-chat", false, false, false,
                PlayerAction.class, Il8n.getRawMessage("said")));
        registerAction(new ActionType("player-command", false, false, false,
                PlayerAction.class, Il8n.getRawMessage("command-run")));
        registerAction(new ActionType("player-death", false, false, false,
                PlayerDeathAction.class, Il8n.getRawMessage("died")));
        registerAction(new ActionType("player-join", false, false, false,
                PlayerAction.class, Il8n.getRawMessage("joined")));
        registerAction(new ActionType("player-kill", false, true, false,
                EntityAction.class, Il8n.getRawMessage("killed")));
        registerAction(new ActionType("player-quit", false, false, false,
                PlayerAction.class, Il8n.getRawMessage("quit")));
        registerAction(new ActionType("player-teleport", false, false, false,
                EntityTravelAction.class, Il8n.getRawMessage("teleported")));
        registerAction(new ActionType("potion-splash", false, false, false,
                PlayerAction.class, Il8n.getRawMessage("potion-throw")));
        registerAction(new ActionType("prism-drain", false, true, true,
                PrismRollbackAction.class, Il8n.getRawMessage("drained")));
        registerAction(new ActionType("prism-extinguish", false, true, true,
                PrismRollbackAction.class, Il8n.getRawMessage("extinguished")));
        registerAction(new ActionType("prism-process", false, false, false,
                PrismProcessAction.class, Il8n.getRawMessage("ran-process")));
        registerAction(new ActionType("prism-rollback", true, false, false,
                PrismRollbackAction.class, Il8n.getRawMessage("rolled-back")));
        registerAction(new ActionType("sheep-eat", false, false, false,
                BlockAction.class, Il8n.getRawMessage("ate")));
        registerAction(new ActionType("sign-change", false, false, true,
                SignAction.class, Il8n.getRawMessage("wrote")));
        registerAction(new ActionType("spawnegg-use", false, false, false,
                UseAction.class, Il8n.getRawMessage("used")));
        registerAction(new ActionType("tnt-explode", false, true, true,
                BlockAction.class, Il8n.getRawMessage("blew-up")));
        registerAction(new ActionType("tnt-prime", false, false, false,
                UseAction.class, Il8n.getRawMessage("primed")));
        registerAction(new ActionType("tree-grow", true, true, true,
                GrowAction.class, Il8n.getRawMessage("grew")));
        registerAction(new ActionType("vehicle-break", false, true, false,
                VehicleAction.class, Il8n.getRawMessage("broke")));
        registerAction(new ActionType("vehicle-enter", false, false, false,
                VehicleAction.class, Il8n.getRawMessage("entered")));
        registerAction(new ActionType("vehicle-exit", false, false, false,
                VehicleAction.class, Il8n.getRawMessage("exited")));
        registerAction(new ActionType("vehicle-place", true, false, false,
                VehicleAction.class, Il8n.getRawMessage("placed")));
        registerAction(new ActionType("water-break", false, true, true,
                BlockAction.class, Il8n.getRawMessage("broke")));
        registerAction(new ActionType("water-bucket", true, true, true,
                BlockChangeAction.class, Il8n.getRawMessage("poured")));
        registerAction(new ActionType("water-flow", true, true, true,
                BlockAction.class, Il8n.getRawMessage("flowed-into")));
        registerAction(new ActionType("world-edit", true, true, true,
                BlockChangeAction.class, Il8n.getRawMessage("edited")));
        registerAction(new ActionType("xp-pickup", false, false, false,
                PlayerAction.class, Il8n.getRawMessage("picked-up")));
    }
}