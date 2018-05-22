package me.botsko.prism.actions;

import org.bukkit.block.BlockState;

public class GrowAction extends BlockAction {

	/**
	 * 
	 * @param state
	 */
	public void setBlock(BlockState state) {
		if (state != null) {

			// TODO: 1.13
			@SuppressWarnings("deprecation")
			byte data = state.getData().getData();

			this.block = state.getType();
			this.block_subid = data;
			this.world_name = state.getWorld().getName();
			this.x = state.getLocation().getBlockX();
			this.y = state.getLocation().getBlockY();
			this.z = state.getLocation().getBlockZ();
		}
	}
}