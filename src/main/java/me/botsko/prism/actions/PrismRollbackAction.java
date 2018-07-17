package me.botsko.prism.actions;

import org.bukkit.block.BlockState;

public class PrismRollbackAction extends BlockChangeAction {

	/**
	 * 
	 * @param oldblock
	 * @param newBlock
	 * @param parent_id
	 */
	public void setBlockChange(BlockState oldblock, BlockState newBlock, long parent_id) {
		// TODO: Why string? Why?
		this.data = String.valueOf(parent_id);
		if (oldblock != null) {
			// TODO: This... is all old block? What?
			byte oldData = oldblock.getData().getData();
			byte newData = oldblock.getData().getData();

			this.old_block = oldblock.getType();
			this.old_block_subid = oldData;
			this.block = oldblock.getType();
			this.block_subid = newData;
		}
	}

	/**
	 * @return the parent_id
	 */
	public long getParentId() {
		return Long.parseLong(this.data);
	}
}