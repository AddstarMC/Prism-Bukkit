package me.botsko.prism.actionlibs;

import java.util.ArrayList;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.bukkit.plugin.Plugin;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.exceptions.InvalidActionException;

public class ActionRegistry {

    /**
	 * 
	 */
    private final TreeMap<String, ActionType> registeredActions = new TreeMap<String, ActionType>();

    /**
	 * 
	 */
    public ActionRegistry() {
        registerPrismDefaultActions();
    }

    /**
     * Register a new action type for event recording, lookups, etc.
     * 
     * @param actionType
     */
    protected void registerAction(ActionType actionType) {
        registeredActions.put( actionType.getName(), actionType );
    }

    /**
     * Register a new action type for event recording, lookups, etc.
     * 
     * @param actionType
     * @throws InvalidActionException
     */
    public void registerCustomAction(Plugin apiPlugin, ActionType actionType) throws InvalidActionException {

        // Is plugin allowed?
        @SuppressWarnings("unchecked")
        final ArrayList<String> allowedPlugins = (ArrayList<String>) Prism.config
                .getList( "prism.tracking.api.allowed-plugins" );
        if( !allowedPlugins.contains( apiPlugin.getName() ) ) { throw new InvalidActionException(
                "Registering action type not allowed. Plugin '" + apiPlugin.getName()
                        + "' is not in list of allowed plugins." ); }

        // Is action type formatted right
        if( TypeUtils.subStrOccurences( actionType.getName(), "-" ) != 2 ) { throw new InvalidActionException(
                "Invalid action type. Custom actions must contain two hyphens." ); }

        // Register custom action type with the db
        Prism.addActionName( actionType.getName() );

        registeredActions.put( actionType.getName(), actionType );

    }

    /**
     * 
     * @return
     */
    public TreeMap<String, ActionType> getRegisteredAction() {
        return registeredActions;
    }

    /**
     * 
     * @param name
     * @return
     */
    public ActionType getAction(String name) {
        return registeredActions.get( name );
    }

    /**
     * 
     * @param name
     * @return
     */
    public ArrayList<ActionType> getActionsByShortname(String name) {
        final ArrayList<ActionType> actions = new ArrayList<ActionType>();
        for ( final Entry<String, ActionType> entry : registeredActions.entrySet() ) {
            // Match either the name or the short name
            if( entry.getValue().getFamilyName().equals( name ) || entry.getValue().getShortName().equals( name )
                    || entry.getValue().getName().equals( name ) ) {
                actions.add( entry.getValue() );
            }
        }
        return actions;
    }

    /**
     * 
     * @return
     */
    public String[] listAll() {
        final String[] names = new String[registeredActions.size()];
        int i = 0;
        for ( final Entry<String, ActionType> entry : registeredActions.entrySet() ) {
            names[i] = entry.getKey();
            i++;
        }
        return names;
    }

    /**
     * 
     * @return
     */
    public ArrayList<String> listActionsThatAllowRollback() {
        final ArrayList<String> names = new ArrayList<String>();
        for ( final Entry<String, ActionType> entry : registeredActions.entrySet() ) {
            if( entry.getValue().canRollback() ) {
                names.add( entry.getKey() );
            }
        }
        return names;
    }

    /**
     * 
     * @return
     */
    public ArrayList<String> listActionsThatAllowRestore() {
        final ArrayList<String> names = new ArrayList<String>();
        for ( final Entry<String, ActionType> entry : registeredActions.entrySet() ) {
            if( entry.getValue().canRestore() ) {
                names.add( entry.getKey() );
            }
        }
        return names;
    }

    /**
	 * 
	 */
    private void registerPrismDefaultActions() {

        registerAction( new ActionType( "block-break", false, true, true, "BlockAction", "broke" ) );
        registerAction( new ActionType( "block-burn", false, true, true, "BlockAction", "burned" ) );
        registerAction( new ActionType( "block-dispense", false, false, false, "ItemStackAction", "dispensed" ) );
        registerAction( new ActionType( "block-fade", false, true, true, "BlockChangeAction", "faded" ) );
        registerAction( new ActionType( "block-fall", false, true, true, "BlockAction", "fell" ) );
        registerAction( new ActionType( "block-form", false, true, true, "BlockChangeAction", "formed" ) );
        registerAction( new ActionType( "block-place", true, true, true, "BlockChangeAction", "placed" ) );
        registerAction( new ActionType( "block-shift", true, false, false, "BlockShift", "moved" ) );
        registerAction( new ActionType( "block-spread", true, true, true, "BlockChangeAction", "grew" ) );
        registerAction( new ActionType( "block-use", false, false, false, "BlockAction", "used" ) );
        registerAction( new ActionType( "bonemeal-use", false, false, false, "UseAction", "used" ) );
        registerAction( new ActionType( "bucket-fill", false, false, false, "PlayerAction", "filled" ) );
        registerAction( new ActionType( "cake-eat", false, false, false, "UseAction", "ate" ) );
        registerAction( new ActionType( "container-access", false, false, false, "BlockAction", "accessed" ) );
        registerAction( new ActionType( "craft-item", false, false, false, "ItemStackAction", "crafted" ) );
        registerAction( new ActionType( "creeper-explode", false, true, true, "BlockAction", "blew up" ) );
        registerAction( new ActionType( "crop-trample", false, true, true, "BlockAction", "trampled" ) );
        registerAction( new ActionType( "dragon-eat", false, true, true, "BlockAction", "ate" ) );
        registerAction( new ActionType( "enchant-item", false, false, false, "ItemStackAction", "enchanted" ) );
        registerAction( new ActionType( "enderman-pickup", false, true, true, "BlockAction", "picked up" ) );
        registerAction( new ActionType( "enderman-place", true, true, true, "BlockAction", "placed" ) );
        registerAction( new ActionType( "entity-break", true, true, true, "BlockAction", "broke" ) );
        registerAction( new ActionType( "entity-dye", false, false, false, "EntityAction", "dyed" ) );
        registerAction( new ActionType( "entity-explode", false, true, true, "BlockAction", "blew up" ) );
        registerAction( new ActionType( "entity-follow", false, false, false, "EntityAction", "lured" ) );
        registerAction( new ActionType( "entity-form", true, true, true, "BlockChangeAction", "formed" ) );
        registerAction( new ActionType( "entity-kill", false, true, false, "EntityAction", "killed" ) );
        registerAction( new ActionType( "entity-leash", true, false, false, "EntityAction", "leashed" ) );
        registerAction( new ActionType( "entity-shear", false, false, false, "EntityAction", "sheared" ) );
        registerAction( new ActionType( "entity-spawn", false, false, false, "EntityAction", "spawned" ) );
        registerAction( new ActionType( "entity-unleash", false, false, false, "EntityAction", "unleashed" ) );
        registerAction( new ActionType( "fireball", false, false, false, null, "ignited" ) );
        registerAction( new ActionType( "fire-spread", true, true, true, "BlockChangeAction", "spread" ) );
        registerAction( new ActionType( "firework-launch", false, false, false, "ItemStackAction", "launched" ) );
        registerAction( new ActionType( "hangingitem-break", false, true, true, "HangingItemAction", "broke" ) );
        registerAction( new ActionType( "hangingitem-place", true, true, true, "HangingItemAction", "hung" ) );
        registerAction( new ActionType( "item-drop", false, true, true, "ItemStackAction", "dropped" ) );
        registerAction( new ActionType( "item-insert", false, true, true, "ItemStackAction", "inserted" ) );
        registerAction( new ActionType( "item-pickup", false, true, true, "ItemStackAction", "picked up" ) );
        registerAction( new ActionType( "item-remove", false, true, true, "ItemStackAction", "removed" ) );
        registerAction( new ActionType( "item-rotate", false, false, false, "UseAction", "turned item" ) );
        registerAction( new ActionType( "lava-break", false, true, true, "BlockAction", "broke" ) );
        registerAction( new ActionType( "lava-bucket", true, true, true, "BlockChangeAction", "poured" ) );
        registerAction( new ActionType( "lava-flow", true, true, true, "BlockAction", "flowed into" ) );
        registerAction( new ActionType( "lava-ignite", false, false, false, null, "ignited" ) );
        registerAction( new ActionType( "leaf-decay", false, true, true, "BlockAction", "decayed" ) );
        registerAction( new ActionType( "lighter", false, false, false, null, "set a fire" ) );
        registerAction( new ActionType( "lightning", false, false, false, null, "ignited" ) );
        registerAction( new ActionType( "mushroom-grow", true, true, true, "GrowAction", "grew" ) );
        registerAction( new ActionType( "player-chat", false, false, false, "PlayerAction", "said" ) );
        registerAction( new ActionType( "player-command", false, false, false, "PlayerAction", "ran command" ) );
        registerAction( new ActionType( "player-death", false, false, false, "PlayerDeathAction", "died" ) );
        registerAction( new ActionType( "player-join", false, false, false, "PlayerAction", "joined" ) );
        registerAction( new ActionType( "player-kill", false, true, false, "EntityAction", "killed" ) );
        registerAction( new ActionType( "player-quit", false, false, false, "PlayerAction", "quit" ) );
        registerAction( new ActionType( "player-teleport", false, false, false, "EntityTravelAction", "teleported" ) );
        registerAction( new ActionType( "potion-splash", false, false, false, "PlayerAction", "threw potion" ) );
        registerAction( new ActionType( "prism-drain", false, true, true, "PrismRollbackAction", "drained" ) );
        registerAction( new ActionType( "prism-extinguish", false, true, true, "PrismRollbackAction", "extinguished" ) );
        registerAction( new ActionType( "prism-process", false, false, false, "PrismProcessAction", "ran process" ) );
        registerAction( new ActionType( "prism-rollback", true, false, false, "PrismRollbackAction", "rolled back" ) );
        registerAction( new ActionType( "sheep-eat", false, false, false, "BlockAction", "ate" ) );
        registerAction( new ActionType( "sign-change", false, false, true, "SignAction", "wrote" ) );
        registerAction( new ActionType( "spawnegg-use", false, false, false, "UseAction", "used" ) );
        registerAction( new ActionType( "tnt-explode", false, true, true, "BlockAction", "blew up" ) );
        registerAction( new ActionType( "tnt-prime", false, false, false, "UseAction", "primed" ) );
        registerAction( new ActionType( "tree-grow", true, true, true, "GrowAction", "grew" ) );
        registerAction( new ActionType( "vehicle-break", false, true, false, "VehicleAction", "broke" ) );
        registerAction( new ActionType( "vehicle-enter", false, false, false, "VehicleAction", "entered" ) );
        registerAction( new ActionType( "vehicle-exit", false, false, false, "VehicleAction", "exited" ) );
        registerAction( new ActionType( "vehicle-place", true, false, false, "VehicleAction", "placed" ) );
        registerAction( new ActionType( "water-break", false, true, true, "BlockAction", "broke" ) );
        registerAction( new ActionType( "water-bucket", true, true, true, "BlockChangeAction", "poured" ) );
        registerAction( new ActionType( "water-flow", true, true, true, "BlockAction", "flowed into" ) );
        registerAction( new ActionType( "world-edit", true, true, true, "BlockChangeAction", "edited" ) );
        registerAction( new ActionType( "xp-pickup", false, false, false, "PlayerAction", "picked up" ) );

    }
}