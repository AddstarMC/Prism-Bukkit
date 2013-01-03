package me.botsko.prism.actions;

public enum ActionType {
	
	/**
	 * ACTION TYPES
	 */
	BLOCK_BREAK(false, true, true, "broke"),
	BLOCK_BURN(false, true, true, "burned"),
	BLOCK_FADE(false, true, true, "faded"),
	BLOCK_FALL(false, true, true, "fell"),
	BLOCK_FORM(false, true, true, "formed"),
	BLOCK_PLACE(true, true, true, "placed"),
	BLOCK_USE(false, false, false, "used"),
	CONTAINER_ACCESS(false, false, false, "accessed"), // isBlockAction
	CREEPER_EXPLODE(false, false, false, "blew up"),
	ENDERMAN_PICKUP(false, true, true, "picked up"),
	ENDERMAN_PLACE(true, true, true, "placed"),
	ENTITY_EXPLODE(false, true, true, "blew up"),
	ENTITY_KILL(false, true, true, "killed"),
	ENTITY_SHEAR(false, false, false, "sheared"),
	FIREBALL(false, false, false, "ignited"),
	FLINT_STEEL(false, false, false, "ignited"),
	ITEM_INSERT(false, false, false, "inserted"),
	ITEM_REMOVE(false, false, false, "removed"),
	LAVA_BUCKET(false, false, false, "poured"),
	LAVA_IGNITE(false, false, false, "ignited"),
	LEAF_DECAY(false, false, false, "decayed"), // isBlockAction
	LIGHTNING(false, false, false, "ignited"),
	MUSHROOM_GROW(true, true, true, "grew"),
	SHEEP_EAT(false, false, false, "ate"), // isEntity
	SIGN_CHANGE(false, false, false, "wrote"),
	TNT_EXPLODE(false, true, true, "blew up"),
	TREE_GROW(false, true, true, "grew"),
	WATER_BUCKET(false, false, false, "poured");
	

	
	/**
	 * Define associated values
	 */
	private boolean doesCreateBlock;
	private boolean canRollback;
	private boolean canRestore;
	private String niceDescription;
	
	
	/**
	 * 
	 * @param actionTypeName
	 * @param canRollback
	 * @param canRestore
	 * @param niceDescription
	 */
	private ActionType(boolean doesCreateBlock, boolean canRollback, boolean canRestore, String niceDescription){
		this.doesCreateBlock = doesCreateBlock;
		this.canRollback = canRollback;
		this.canRestore = canRestore;
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
	public String getNiceDescription() {
		return niceDescription;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isBlockAction(){
		return (getActionType().contains("block") || getActionType().equals("leaf-decay") || getActionType().contains("container") );
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isEntityAction(){
		return (getActionType().contains("entity") || getActionType().contains("eat") );
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isItemStackAction(){
		return getActionType().contains("item");
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isSignAction(){
		return getActionType().contains("sign");
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