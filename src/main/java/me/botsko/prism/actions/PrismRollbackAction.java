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
			this.old_block = oldblock.getType();
			this.old_block_data = oldblock.getBlockData();
			this.block = oldblock.getType();

			// TODO: This is using oldblock when it looks like it should use newBlock,
			// but this is how it was when I found it. Test later.
			this.block_data = oldblock.getBlockData();
		}
	}

	/**
	 * @return the parent_id
	 */
	public long getParentId() {
		return Long.parseLong(this.data);
	}
}