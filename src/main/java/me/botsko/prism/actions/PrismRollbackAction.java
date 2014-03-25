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
            this.old_block_id = oldblock.getTypeId();
            this.old_block_subid = oldblock.getRawData();
            this.block_id = oldblock.getTypeId();
            this.block_subid = oldblock.getRawData();
        }
    }

    /**
     * @return the parent_id
     */
    public int getParentId() {
        return Integer.parseInt( this.data );
    }
}