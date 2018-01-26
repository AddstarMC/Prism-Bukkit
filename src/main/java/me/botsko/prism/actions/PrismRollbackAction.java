package me.botsko.prism.actions;

import org.bukkit.block.BlockState;

public class PrismRollbackAction extends BlockChangeAction {

    /**
     * 
     * @param oldblock
     * @param newBlock
     * @param parent_id
     */
    public void setBlockChange(BlockState oldblock, BlockState newBlock, int parent_id) {
        this.data = "" + parent_id;
        if( oldblock != null ) {
        	// TODO: This... is all old block? What?
        	// TODO: 1.13
        	@SuppressWarnings("deprecation")
			byte oldData = oldblock.getData().getData();
        	@SuppressWarnings("deprecation")
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
    public int getParentId() {
        return Integer.parseInt( this.data );
    }
}