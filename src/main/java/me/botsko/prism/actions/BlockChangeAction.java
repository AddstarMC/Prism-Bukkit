package me.botsko.prism.actions;

import org.bukkit.Location;

public class BlockChangeAction extends GenericAction {

	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public BlockChangeAction( String action_type, Location loc, int oldId, byte oldSubid, int newId, byte newSubid, String player ){
		
		super(action_type, player);
		
		if(newId != 0){
			// Water/Lava placement always turns into stationary blocks, and a rollback would
			// fail because we wouldn't detect the same block placed on rollback. So,
			// we just force record the block as stationary.
			// https://snowy-evening.com/botsko/prism/297/
			if( this.type.getName().equals("block-place") && (newId == 8 || newId == 10) ){
				newId = (newId == 8 ? 9 : 11);
			}
			this.block_id = newId;
			this.block_subid = newSubid;
		}
		if(oldId != 0){
			this.old_block_id = oldId;
			this.old_block_subid = oldSubid;
		}
		if(loc != null){
			this.world_name = loc.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
		}
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
	}
	
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		if(this.getType().getName().equals("block-fade")){
			name += materialAliases.getItemStackAliasById(this.old_block_id, this.old_block_subid);
		} else {
			name += materialAliases.getItemStackAliasById(this.block_id, this.block_subid);
		}
		return name;
	}
	
	
	/**
	 * 
	 * @author botskonet
	 */
	public class BlockChangeActionData {
		public int old_id;
		public byte old_subid;
	}
}