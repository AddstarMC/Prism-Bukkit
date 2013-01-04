package me.botsko.prism.actions;

public enum ActionType {
	
	/**
	 * ACTION TYPES
	 */
	BLOCK_BREAK(false, true, true, "block", "broke"),
	BLOCK_BURN(false, true, true, "block", "burned"),
	BLOCK_FADE(false, true, true, "block", "faded"),
	BLOCK_FALL(false, true, true, "block", "fell"),
	BLOCK_FORM(false, true, true, "block", "formed"),
	BLOCK_PLACE(true, true, true, "block", "placed"),
	BLOCK_USE(false, false, false, "block", "used"),
	CONTAINER_ACCESS(false, false, false, "block", "accessed"),
	CREEPER_EXPLODE(false, false, false, "block", "blew up"),
	ENDERMAN_PICKUP(false, true, true, "block", "picked up"),
	ENDERMAN_PLACE(true, true, true, "block", "placed"),
	ENTITY_BREAK(true, true, true, "block", "broke"),
	ENTITY_EXPLODE(false, true, true, "block", "blew up"),
	ENTITY_KILL(false, true, true, "entity", "killed"),
	ENTITY_SHEAR(false, false, false, "entity", "sheared"),
	FIREBALL(false, false, false, null, "ignited"),
	FLINT_STEEL(false, false, false, null, "ignited"),
	HANGINGITEM_BREAK(false, true, true, "hangingitem", "broke"),
	HANGINGITEM_PLACE(true, true, true, "hangingitem", "hung"),
	ITEM_DROP(false, false, false, "itemstack", "dropped"),
	ITEM_INSERT(false, false, false, "itemstack", "inserted"),
	ITEM_PICKUP(false, false, false, "itemstack", "picked up"),
	ITEM_REMOVE(false, false, false, "itemstack", "removed"),
	LAVA_BUCKET(false, false, false, null, "poured"),
	LAVA_IGNITE(false, false, false, null, "ignited"),
	LEAF_DECAY(false, false, false, "block", "decayed"),
	LIGHTNING(false, false, false, null, "ignited"),
	MUSHROOM_GROW(true, true, true, "block", "grew"),
	PLAYER_DEATH(false, false, false, "playerdeath", "died"),
	SHEEP_EAT(false, false, false, "entity", "ate"),
	SIGN_CHANGE(false, false, false, "signchange", "wrote"),
	TNT_EXPLODE(false, true, true, "block", "blew up"),
	TREE_GROW(false, true, true, "block", "grew"),
	WATER_BUCKET(false, false, false, null, "poured");
	

	
	/**
	 * Define associated values
	 */
	private boolean doesCreateBlock;
	private boolean canRollback;
	private boolean canRestore;
	private String handler;
	private String niceDescription;
	
	
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
	}

	
	/**
	 * @return the canRollback
	 */
	public boolean isCanRollback() {
		return canRollback;
	}

	
	/**
	 * @return the canRestore
	 */
	public boolean isCanRestore() {
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
	 * @return
	 */
	public boolean isBlockAction(){
		return (getHandler() != null && getHandler().equals("block"));
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isEntityAction(){
		return (getHandler() != null && getHandler().equals("entity"));
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isItemStackAction(){
		return (getHandler() != null && getHandler().equals("itemstack"));
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isPlayerDeathAction(){
		return (getHandler() != null && getHandler().equals("playerdeath"));
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isSignAction(){
		return (getHandler() != null && getHandler().equals("signchange"));
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
		return valueOf(_seek_type);
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getActionType(){
		return this.name().toLowerCase().replace("_", "-");
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getName(){
		return this.name();
	}
}