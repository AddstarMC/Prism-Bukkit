package me.botsko.prism.actions;

import org.bukkit.block.BlockState;


public class PrismRollbackAction extends GenericAction {
	
	/**
	 * 
	 * @author botskonet
	 * @deprecated
	 */
	public class PrismRollbackActionData {
		public int originalBlock_id;
		public int originalBlock_subid;
		public int newBlock_id;
		public int newBlock_subid;
		public int parent_id;
	}
	
	/**
	 * 
	 */
	protected PrismRollbackActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public void setBlockChange(  BlockState oldblock, BlockState newBlock, int parent_id ){
		
		actionData = new PrismRollbackActionData();
		
		if(oldblock != null){
			this.old_block_id = oldblock.getTypeId();
			this.old_block_subid = oldblock.getRawData();
			this.block_id = oldblock.getTypeId();
			this.block_subid = oldblock.getRawData();

			actionData.parent_id = parent_id;
		}
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setObjectFromData();
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromObject(){
		data = gson.toJson(actionData);
	}
	
	
	/**
	 * 
	 */
	protected void setObjectFromData(){
		if(data != null && data.contains("{")){
			actionData = gson.fromJson(data, PrismRollbackActionData.class);
			this.old_block_id = actionData.originalBlock_id;
			this.old_block_subid = (byte)actionData.originalBlock_subid;
			this.block_id = actionData.newBlock_id;
			this.block_subid =(byte) actionData.newBlock_subid;
		}
	}

	
	/**
	 * @return the parent_id
	 */
	public int getParentId() {
		return actionData.parent_id;
	}
}