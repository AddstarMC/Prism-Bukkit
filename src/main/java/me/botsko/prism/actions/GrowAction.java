package me.botsko.prism.actions;

import org.bukkit.block.BlockState;

public class GrowAction extends BlockAction {

	/**
	 * @param blockstate
	 */
	public void setBlock(BlockState blockstate) {
		if (blockstate != null) {
			this.block_id = blockstate.getTypeId();
			this.block_subid = blockstate.getData().getData();
			this.world_name = blockstate.getWorld().getName();
			this.x = blockstate.getLocation().getBlockX();
			this.y = blockstate.getLocation().getBlockY();
			this.z = blockstate.getLocation().getBlockZ();
		}
	}
}
