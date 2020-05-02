package me.botsko.prism.actionlibs;

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

        registerAction(new ActionType("block-break", false, true, true, BlockAction.class, "broke"));
        registerAction(new ActionType("block-burn", false, true, true, BlockAction.class, "burned"));
        registerAction(new ActionType("block-dispense", false, false, false, ItemStackAction.class, "dispensed"));
        registerAction(new ActionType("block-fade", false, true, true, BlockChangeAction.class, "faded"));
        registerAction(new ActionType("block-fall", false, true, true, BlockAction.class, "fell"));
        registerAction(new ActionType("block-form", false, true, true, BlockChangeAction.class, "formed"));
        registerAction(new ActionType("block-place", true, true, true, BlockChangeAction.class, "placed"));
        registerAction(new ActionType("block-shift", true, false, false, BlockShiftAction.class, "moved"));
        registerAction(new ActionType("block-spread", true, true, true, BlockChangeAction.class, "grew"));
        registerAction(new ActionType("block-use", false, false, false, BlockAction.class, "used"));
        registerAction(new ActionType("bonemeal-use", false, false, false, UseAction.class, "used"));
        registerAction(new ActionType("bucket-fill", false, false, false, PlayerAction.class, "filled"));
        registerAction(new ActionType("cake-eat", false, false, false, UseAction.class, "ate"));
        registerAction(new ActionType("container-access", false, false, false, BlockAction.class, "accessed"));
        registerAction(new ActionType("craft-item", false, false, false, ItemStackAction.class, "crafted"));
        registerAction(new ActionType("creeper-explode", false, true, true, BlockAction.class, "blew up"));
        registerAction(new ActionType("crop-trample", false, true, true, BlockAction.class, "trampled"));
        registerAction(new ActionType("dragon-eat", false, true, true, BlockAction.class, "ate"));
        registerAction(new ActionType("enchant-item", false, false, false, ItemStackAction.class, "enchanted"));
        registerAction(new ActionType("enderman-pickup", false, true, true, BlockAction.class, "picked up"));
        registerAction(new ActionType("enderman-place", true, true, true, BlockAction.class, "placed"));
        registerAction(new ActionType("entity-break", true, true, true, BlockAction.class, "broke"));
        registerAction(new ActionType("entity-dye", false, false, false, EntityAction.class, "dyed"));
        registerAction(new ActionType("entity-explode", false, true, true, BlockAction.class, "blew up"));
        registerAction(new ActionType("entity-follow", false, false, false, EntityAction.class, "lured"));
        registerAction(new ActionType("entity-form", true, true, true, BlockChangeAction.class, "formed"));
        registerAction(new ActionType("entity-kill", false, true, false, EntityAction.class, "killed"));
        registerAction(new ActionType("entity-leash", true, false, false, EntityAction.class, "leashed"));
        registerAction(new ActionType("entity-shear", false, false, false, EntityAction.class, "sheared"));
        registerAction(new ActionType("entity-spawn", false, false, false, EntityAction.class, "spawned"));
        registerAction(new ActionType("entity-unleash", false, false, false, EntityAction.class, "unleashed"));
        registerAction(new ActionType("fireball", false, false, false, BlockAction.class, "ignited"));
        registerAction(new ActionType("fire-spread", true, true, true, BlockChangeAction.class, "spread"));
        registerAction(new ActionType("firework-launch", false, false, false, ItemStackAction.class, "launched"));
        registerAction(new ActionType("hangingitem-break", false, true, true, HangingItemAction.class, "broke"));
        registerAction(new ActionType("hangingitem-place", true, true, true, HangingItemAction.class, "hung"));
        registerAction(new ActionType("item-drop", false, true, true, ItemStackAction.class, "dropped"));
        registerAction(new ActionType("item-insert", false, true, true, ItemStackAction.class, "inserted"));
        registerAction(new ActionType("item-pickup", false, true, true, ItemStackAction.class, "picked up"));
        registerAction(new ActionType("item-remove", false, true, true, ItemStackAction.class, "removed"));
        registerAction(new ActionType("item-rotate", false, false, false, UseAction.class, "turned item"));
        registerAction(new ActionType("lava-break", false, true, true, BlockAction.class, "broke"));
        registerAction(new ActionType("lava-bucket", true, true, true, BlockChangeAction.class, "poured"));
        registerAction(new ActionType("lava-flow", true, true, true, BlockAction.class, "flowed into"));
        registerAction(new ActionType("lava-ignite", false, false, false, BlockAction.class, "ignited"));
        registerAction(new ActionType("leaf-decay", false, true, true, BlockAction.class, "decayed"));
        registerAction(new ActionType("lighter", false, false, false, BlockAction.class, "set a fire"));
        registerAction(new ActionType("lightning", false, false, false, BlockAction.class, "ignited"));
        registerAction(new ActionType("mushroom-grow", true, true, true, GrowAction.class, "grew"));
        registerAction(new ActionType("player-chat", false, false, false, PlayerAction.class, "said"));
        registerAction(new ActionType("player-command", false, false, false, PlayerAction.class, "ran command"));
        registerAction(new ActionType("player-death", false, false, false, PlayerDeathAction.class, "died"));
        registerAction(new ActionType("player-join", false, false, false, PlayerAction.class, "joined"));
        registerAction(new ActionType("player-kill", false, true, false, EntityAction.class, "killed"));
        registerAction(new ActionType("player-quit", false, false, false, PlayerAction.class, "quit"));
        registerAction(new ActionType("player-teleport", false, false, false, EntityTravelAction.class, "teleported"));
        registerAction(new ActionType("potion-splash", false, false, false, PlayerAction.class, "threw potion"));
        registerAction(new ActionType("prism-drain", false, true, true, PrismRollbackAction.class, "drained"));
        registerAction(new ActionType("prism-extinguish", false, true, true,
                PrismRollbackAction.class, "extinguished"));
        registerAction(new ActionType("prism-process", false, false, false, PrismProcessAction.class, "ran process"));
        registerAction(new ActionType("prism-rollback", true, false, false, PrismRollbackAction.class, "rolled back"));
        registerAction(new ActionType("sheep-eat", false, false, false, BlockAction.class, "ate"));
        registerAction(new ActionType("sign-change", false, false, true, SignAction.class, "wrote"));
        registerAction(new ActionType("spawnegg-use", false, false, false, UseAction.class, "used"));
        registerAction(new ActionType("tnt-explode", false, true, true, BlockAction.class, "blew up"));
        registerAction(new ActionType("tnt-prime", false, false, false, UseAction.class, "primed"));
        registerAction(new ActionType("tree-grow", true, true, true, GrowAction.class, "grew"));
        registerAction(new ActionType("vehicle-break", false, true, false, VehicleAction.class, "broke"));
        registerAction(new ActionType("vehicle-enter", false, false, false, VehicleAction.class, "entered"));
        registerAction(new ActionType("vehicle-exit", false, false, false, VehicleAction.class, "exited"));
        registerAction(new ActionType("vehicle-place", true, false, false, VehicleAction.class, "placed"));
        registerAction(new ActionType("water-break", false, true, true, BlockAction.class, "broke"));
        registerAction(new ActionType("water-bucket", true, true, true, BlockChangeAction.class, "poured"));
        registerAction(new ActionType("water-flow", true, true, true, BlockAction.class, "flowed into"));
        registerAction(new ActionType("world-edit", true, true, true, BlockChangeAction.class, "edited"));
        registerAction(new ActionType("xp-pickup", false, false, false, PlayerAction.class, "picked up"));
    }
}