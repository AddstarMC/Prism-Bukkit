package me.botsko.prism.actionlibs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.bukkit.plugin.Plugin;

import me.botsko.prism.Prism;
import me.botsko.prism.exceptions.InvalidActionException;
import me.botsko.prism.utils.TypeUtils;

public class ActionRegistry {
	
	
	/**
	 * 
	 */
	private HashMap<String,ActionType> registeredActions = new HashMap<String,ActionType>();
	
	
	/**
	 * 
	 */
	public ActionRegistry(){
		registerPrismDefaultActions();
	}
	
	
	/**
	 * Register a new action type for event recording, lookups, etc.
	 * @param actionType
	 */
	protected void registerAction( ActionType actionType ){
		registeredActions.put(actionType.getName(), actionType);
	}
	
	
	/**
	 * Register a new action type for event recording, lookups, etc.
	 * @param actionType
	 * @throws InvalidActionException 
	 */
	public void registerCustomAction( Plugin apiPlugin, ActionType actionType ) throws InvalidActionException{
		
		// Is plugin allowed?
		@SuppressWarnings("unchecked")
		ArrayList<String> allowedPlugins = (ArrayList<String>) Prism.config.getList("prism.tracking.api.allowed-plugins");
		if( !allowedPlugins.contains( apiPlugin.getName() ) ){
			throw new InvalidActionException("Registering action type not allowed. Plugin '" + apiPlugin.getName() + "' is not in list of allowed plugins.");
		}
		
		// Is action type formatted right
		if( TypeUtils.subStrOccurences(actionType.getName(), "-") != 2 ){
			throw new InvalidActionException("Invalid action type. Custom actions must contain two hyphens.");
		}
		
		registeredActions.put(actionType.getName(), actionType);
		
	}
	
	
	/**
	 * 
	 * @return
	 */
	public HashMap<String,ActionType> getRegisteredAction(){
		return registeredActions;
	}
	
	
	/**
	 * 
	 * @param name
	 * @return
	 */
	public ActionType getAction( String name ){
		return registeredActions.get(name);
	}
	
	
	/**
	 * 
	 * @param name
	 * @return
	 */
	public ArrayList<ActionType> getActionsByShortname( String name ){
		ArrayList<ActionType> actions = new ArrayList<ActionType>();
		for (Entry<String,ActionType> entry : registeredActions.entrySet()){
			if(entry.getValue().getShortName().equals(name)){
				actions.add(entry.getValue());
			}
		}
		return actions;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String[] listAll(){
		String[] names = new String[ registeredActions.size() ];
		int i = 0;
		for (Entry<String,ActionType> entry : registeredActions.entrySet()){
			names[i] = entry.getKey();
			i++;
		}
		return names;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String[] listActionsThatAllowRollback(){
		String[] names = new String[ registeredActions.size() ];
		int i = 0;
		for (Entry<String,ActionType> entry : registeredActions.entrySet()){
			if(entry.getValue().canRollback()){
				names[i] = entry.getKey();
			}
			i++;
		}
		return names;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String[] listActionsThatAllowRestore(){
		String[] names = new String[ registeredActions.size() ];
		int i = 0;
		for (Entry<String,ActionType> entry : registeredActions.entrySet()){
			if(entry.getValue().canRestore()){
				names[i] = entry.getKey();
			}
			i++;
		}
		return names;
	}
	
	
	/**
	 * 
	 */
	private void registerPrismDefaultActions(){
		
		registerAction( new ActionType( "block-break", false, true, true, "block", "broke") );
		registerAction( new ActionType( "block-burn", false, true, true, "block", "burned") );
		registerAction( new ActionType( "block-fade", false, true, true, "blockchange", "faded") );
		registerAction( new ActionType( "block-fall", false, true, true, "block", "fell") );
		registerAction( new ActionType( "block-form", false, true, true, "blockchange", "formed") );
		registerAction( new ActionType( "block-place", true, true, true, "blockchange", "placed") );
		registerAction( new ActionType( "block-shift", true, false, false, "blockshift", "moved") );
		registerAction( new ActionType( "block-spread", true, true, true, "blockchange", "grew") );
		registerAction( new ActionType( "block-use", false, false, false, "block", "used") );
		registerAction( new ActionType( "bonemeal-use", false, false, false, "use", "used") );
		registerAction( new ActionType( "bucket-fill", false, false, false, "player", "filled") );
		registerAction( new ActionType( "container-access", false, false, false, "block", "accessed") );
		registerAction( new ActionType( "craft-item", false, false, false, "itemstack", "crafted") );
		registerAction( new ActionType( "creeper-explode", false, true, true, "block", "blew up") );
		registerAction( new ActionType( "crop-trample", false, true, true, "block", "trampled") );
		registerAction( new ActionType( "enchant-item", false, false, false, "itemstack", "enchanted") );
		registerAction( new ActionType( "enderman-pickup", false, true, true, "block", "picked up") );
		registerAction( new ActionType( "enderman-place", true, true, true, "block", "placed") );
		registerAction( new ActionType( "entity-break", true, true, true, "block", "broke") );
		registerAction( new ActionType( "entity-dye", false, false, false, "entity", "dyed") );
		registerAction( new ActionType( "entity-explode", false, true, true, "block", "blew up") );
		registerAction( new ActionType( "entity-follow", false, false, false, "entity", "lured") );
		registerAction( new ActionType( "entity-form", true, true, true, "blockchange", "formed") );
		registerAction( new ActionType( "entity-kill", false, true, false, "entity", "killed") );
		registerAction( new ActionType( "entity-shear", false, false, false, "entity", "sheared") );
		registerAction( new ActionType( "entity-spawn", false, false, false, "entity", "spawned") );
		registerAction( new ActionType( "fireball", false, false, false, null, "ignited") );
		registerAction( new ActionType( "hangingitem-break", false, true, true, "hangingitem", "broke") );
		registerAction( new ActionType( "hangingitem-place", true, true, true, "hangingitem", "hung") );
		registerAction( new ActionType( "item-drop", false, false, false, "itemstack", "dropped") );
		registerAction( new ActionType( "item-insert", false, true, true, "itemstack", "inserted") );
		registerAction( new ActionType( "item-pickup", false, false, false, "itemstack", "picked up") );
		registerAction( new ActionType( "item-remove", false, true, true, "itemstack", "removed") );
		registerAction( new ActionType( "lava-break", false, true, true, "block", "broke") );
		registerAction( new ActionType( "lava-bucket", true, true, true, "blockchange", "poured") );
		registerAction( new ActionType( "lava-flow", true, true, true, "block", "flowed into") );
		registerAction( new ActionType( "lava-ignite", false, false, false, null, "ignited") );
		registerAction( new ActionType( "leaf-decay", false, true, true, "block", "decayed") );
		registerAction( new ActionType( "lighter", false, false, false, null, "set a fire") );
		registerAction( new ActionType( "lightning", false, false, false, null, "ignited") );
		registerAction( new ActionType( "mushroom-grow", true, true, true, "grow", "grew") );
		registerAction( new ActionType( "player-chat", false, false, false, "command", "said") );
		registerAction( new ActionType( "player-command", false, false, false, "command", "ran command") );
		registerAction( new ActionType( "player-death", false, false, false, "playerdeath", "died") );
		registerAction( new ActionType( "player-join", false, false, false, "player", "joined") );
		registerAction( new ActionType( "player-kill", false, true, false, "entity", "killed") );
		registerAction( new ActionType( "player-quit", false, false, false, "player", "quit") );
		registerAction( new ActionType( "player-teleport", false, false, false, "entitytravel", "teleported") );
		registerAction( new ActionType( "poition-splash", false, false, false, "player", "threw potion") );
		registerAction( new ActionType( "prism-drain", false, true, true, "prismrollback", "drained") );
		registerAction( new ActionType( "prism-process", false, false, false, "prismprocess", "ran process") );
		registerAction( new ActionType( "prism-rollback", true, false, false, "prismrollback", "rolled back") );
		registerAction( new ActionType( "sheep-eat", false, false, false, "block", "ate") );
		registerAction( new ActionType( "sign-change", false, false, true, "signchange", "wrote") );
		registerAction( new ActionType( "spawnegg-use", false, false, false, "use", "used") );
		registerAction( new ActionType( "tnt-explode", false, true, true, "block", "blew up") );
		registerAction( new ActionType( "tnt-prime", false, false, false, "use", "primed") );
		registerAction( new ActionType( "tree-grow", true, true, true, "grow", "grew") );
		registerAction( new ActionType( "water-break", false, true, true, "block", "broke") );
		registerAction( new ActionType( "water-bucket", true, true, true, "blockchange", "poured") );
		registerAction( new ActionType( "water-flow", true, true, true, "block", "flowed into") );
		registerAction( new ActionType( "world-edit", true, true, true, "worldedit", "edited") );
		registerAction( new ActionType( "xp-pickup", false, false, false, "player", "picked up") );
		
	}
}