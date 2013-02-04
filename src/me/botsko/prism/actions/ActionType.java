package me.botsko.prism.actions;

import java.util.ArrayList;

public enum ActionType {
	
	/**
	 * ACTION TYPES
	 */
	BLOCK_BREAK(false, true, true, "block", "broke"),
	BLOCK_BURN(false, true, true, "block", "burned"),
	BLOCK_FADE(false, true, true, "blockchange", "faded"),
	BLOCK_FALL(false, true, true, "block", "fell"),
	BLOCK_FORM(false, true, true, "blockchange", "formed"),
	BLOCK_PLACE(true, true, true, "blockchange", "placed"),
	BLOCK_SHIFT(true, false, false, "blockshift", "moved"),
	BLOCK_SPREAD(true, true, true, "blockchange", "grew"),
	BLOCK_USE(false, false, false, "block", "used"),
	BONEMEAL_USE(false, false, false, "use", "used"),
	CONTAINER_ACCESS(false, false, false, "block", "accessed"),
	CREEPER_EXPLODE(false, true, true, "block", "blew up"),
	CROP_TRAMPLE(false, true, true, "block", "trampled"),
	ENDERMAN_PICKUP(false, true, true, "block", "picked up"),
	ENDERMAN_PLACE(true, true, true, "block", "placed"),
	ENTITY_BREAK(true, true, true, "block", "broke"),
	ENTITY_EXPLODE(false, true, true, "block", "blew up"),
	ENTITY_FOLLOW(false, false, false, "entity", "lured"),
	ENTITY_KILL(false, true, true, "entity", "killed"),
	ENTITY_SHEAR(false, false, false, "entity", "sheared"),
	ENTITY_SPAWN(false, false, false, "entity", "spawned"),
	FIREBALL(false, false, false, null, "ignited"),
	HANGINGITEM_BREAK(false, true, true, "hangingitem", "broke"),
	HANGINGITEM_PLACE(true, true, true, "hangingitem", "hung"),
	ITEM_DROP(false, false, false, "itemstack", "dropped"),
	ITEM_INSERT(false, false, true, "itemstack", "inserted"),
	ITEM_PICKUP(false, false, false, "itemstack", "picked up"),
	ITEM_REMOVE(false, true, false, "itemstack", "removed"),
	LAVA_BREAK(false, true, true, "block", "broke"),
	LAVA_BUCKET(true, true, true, "blockchange", "poured"),
	LAVA_FLOW(true, true, true, "block", "flowed into"),
	LAVA_IGNITE(false, false, false, null, "ignited"),
	LEAF_DECAY(false, true, true, "block", "decayed"),
	LIGHTER(false, false, false, null, "set a fire"),
	LIGHTNING(false, false, false, null, "ignited"),
	MUSHROOM_GROW(true, true, true, "grow", "grew"),
	PLAYER_CHAT(false, false, false, "command", "said"),
	PLAYER_COMMAND(false, false, false, "command", "ran command"),
	PLAYER_DEATH(false, false, false, "playerdeath", "died"),
	PLAYER_JOIN(false, false, false, "player", "joined"),
	PLAYER_QUIT(false, false, false, "player", "quit"),
	PLAYER_TELEPORT(false, false, false, "entitytravel", "teleported"),
	POTION_SPLASH(false, false, false, "player", "threw potion"),
	PRISM_DRAIN(false, true, true, "prismrollback", "drained"),
	PRISM_PROCESS(false, false, false, "prismprocess", "ran process"),
	PRISM_ROLLBACK(true, false, false, "prismrollback", "rolled back"),
	SHEEP_EAT(false, false, false, "block", "ate"),
	SIGN_CHANGE(false, false, true, "signchange", "wrote"),
	SPAWNEGG_USE(false, false, false, "use", "used"),
	TNT_EXPLODE(false, true, true, "block", "blew up"),
	TNT_PRIME(false, false, false, "use", "primed"),
	TREE_GROW(true, true, true, "grow", "grew"),
	WATER_BREAK(false, true, true, "block", "broke"),
	WATER_BUCKET(true, true, true, "blockchange", "poured"),
	WATER_FLOW(true, true, true, "block", "flowed into"),
	WORLD_EDIT(true, true, true, "worldedit", "edited"),
	XP_PICKUP(false, false, false, "player", "picked up");
	
	
	/**
	 * Define associated values
	 */
	private boolean doesCreateBlock;
	private boolean canRollback;
	private boolean canRestore;
	private String handler;
	private String niceDescription;
	private String actionTypeName;
	
	
	/**
	 * 
	 * @param actionTypeName
	 * @param canRollback
	 * @param canRestore
	 * @param niceDescription
	 */
	private ActionType(boolean doesCreateBlock, boolean canRollback, boolean canRestore, String handler, String niceDescription){
		this.doesCreateBlock = doesCreateBlock;
		this.canRollback = canRollback;
		this.canRestore = canRestore;
		this.handler = handler;
		this.niceDescription = niceDescription;
		this.actionTypeName = this.name().toLowerCase().replace("_", "-");
	}

	
	/**
	 * @return the canRollback
	 */
	public boolean canRollback() {
		return canRollback;
	}

	
	/**
	 * @return the canRestore
	 */
	public boolean canRestore() {
		return canRestore;
	}

	
	/**
	 * @return the niceDescription
	 */
	public String getHandler() {
		return handler;
	}
	
	
	/**
	 * @return the niceDescription
	 */
	public String getNiceDescription() {
		return niceDescription;
	}
	
	
	/**
	 * 
	 * @param handler
	 * @return
	 */
	public boolean requiresHandler( String handler ){
		return (getHandler() != null && getHandler().equals(handler));
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean doesCreateBlock(){
		return doesCreateBlock;
	}
	
	
	/**
	 * 
	 * @param action
	 * @return
	 */
	public static ActionType getByActionType(String action){
		String _seek_type = action.toUpperCase().replace("-", "_");
		for (ActionType me : ActionType.values()) {
	        if (me.name().equalsIgnoreCase(_seek_type))
	            return me;
	    }
	    return null;
	}
	
	
	/**
	 * 
	 * @param action
	 * @return
	 */
	public static ArrayList<ActionType> getByActionsType(String action){
		ArrayList<ActionType> actions = new ArrayList<ActionType>();
		String _seek_type = action.toUpperCase().replace("-", "_");
		for (ActionType me : ActionType.values()) {
	        if (me.name().equalsIgnoreCase(_seek_type) || me.getActionShortType().equals(action))
	        	actions.add(me);
	    }
	    return actions;
	}
	
	
	/**
	 * 
	 * @param action
	 * @return
	 */
	public static ArrayList<ActionType> getCanRollbackActionTypes(){
		ArrayList<ActionType> canRollback = new ArrayList<ActionType>();
		for (ActionType me : ActionType.values()) {
	        if(me.canRestore()){
	        	canRollback.add(me);
	        }
	    }
	    return canRollback;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionType(){
		return actionTypeName;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionShortType(){
		String[] _tmp = this.name().toLowerCase().split("_");
		if(_tmp.length == 2){
			return _tmp[1];
		}
		return this.name().toLowerCase();
	}
	
	
	/**
	 * Returns whether or not an action type should also
	 * trigger a restore action after an applier.
	 * 
	 * This is a pretty inefficient way to define the 
	 * relationships but it's really the only way I 
	 * can think of atm.
	 * 
	 * @param at
	 * @return
	 */
	public boolean shouldTriggerRestoreFor(ActionType at){
		
		// Actions that should trigger sign changes
		if(at.equals(ActionType.SIGN_CHANGE)){
			switch(this){
				case BLOCK_BREAK:
				case BLOCK_BURN:
				case CREEPER_EXPLODE:
				case ENDERMAN_PICKUP:
				case TNT_EXPLODE:
					return true;
				default:
					return false;
			}
		}
		return false;
	}
	
	
	/**
	 * Returns whether or not an action type should also
	 * trigger a rollback action after an applier.
	 * 
	 * This is a pretty inefficient way to define the 
	 * relationships but it's really the only way I 
	 * can think of atm.
	 * 
	 * @param at
	 * @return
	 */
	public boolean shouldTriggerRollbackFor(ActionType at){
		
//		// Actions that should trigger item removal rollback
//		if(at.equals(ActionType.ITEM_REMOVE)){
//			switch(this){
//				case BLOCK_BREAK:
//				case BLOCK_BURN:
//				case CREEPER_EXPLODE:
//				case TNT_EXPLODE:
//					return true;
//				default:
//					return false;
//			}
//		}
		return false;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getName(){
		return this.name();
	}
}