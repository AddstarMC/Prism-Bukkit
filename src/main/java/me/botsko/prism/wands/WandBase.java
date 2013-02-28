package me.botsko.prism.wands;

public abstract class WandBase {

	
	/**
	 * 
	 */
	protected boolean item_given = false;
	
	/**
	 * 
	 */
	protected String wand_mode;
	
	/**
	 * 
	 */
	protected int item_id = 0;
	
	/**
	 * 
	 */
	protected byte item_subid = 0;
	

	/**
	 * 
	 */
	public void setItemWasGiven(boolean given) {
		this.item_given = given;
	}


	/**
	 * 
	 */
	public boolean itemWasGiven() {
		return item_given;
	}
	
	
	/**
	 * 
	 * @param mode
	 */
	public void setWandMode( String mode ){
		wand_mode = mode;
	}
	
	
	/**
	 * 
	 * @param mode
	 */
	public String getWandMode(){
		return wand_mode;
	}


	/**
	 * @return the item_id
	 */
	public int getItemId() {
		return item_id;
	}


	/**
	 * @param item_id the item_id to set
	 */
	public void setItemId(int item_id) {
		this.item_id = item_id;
	}


	/**
	 * @return the item_subid
	 */
	public byte getItemSubId() {
		return item_subid;
	}


	/**
	 * @param item_subid the item_subid to set
	 */
	public void setItemSubId(byte item_subid) {
		this.item_subid = item_subid;
	}
	
	
	/**
	 * 
	 * @param key
	 */
	public void serItemFromKey( String key ){
		if(key.contains(":")){
			String[] toolKeys = key.split(":");
			item_id = Integer.parseInt(toolKeys[0]);
			item_subid = Byte.parseByte(toolKeys[1]);
		}
	}
}
