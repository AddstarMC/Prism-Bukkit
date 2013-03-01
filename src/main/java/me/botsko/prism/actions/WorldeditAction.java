package me.botsko.prism.actions;

import org.bukkit.Location;


public class WorldeditAction extends GenericAction {

	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public WorldeditAction( String action_type, Location loc, int originalBlock_id, int originalBlock_subid, int newBlock_id, int newBlock_subid, String playername ){
		
		super(action_type, playername);
		
		if(playername != null){
			this.old_block_id = originalBlock_id;
			this.old_block_subid = (byte) originalBlock_subid;
			this.block_id = newBlock_id;
			this.block_subid = (byte) newBlock_subid;
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
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		name += materialAliases.getItemStackAliasById(old_block_id, old_block_subid);
		name += " to ";
		name += materialAliases.getItemStackAliasById(block_id, block_subid);
		return name;
	}
}