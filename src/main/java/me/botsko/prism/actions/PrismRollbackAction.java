package me.botsko.prism.actions;

import org.bukkit.block.BlockState;

public class PrismRollbackAction extends BlockChangeAction {

    private long parent_id;

    /**
     * @param oldblock
     * @param newBlock
     * @param parent_id
     */
    public void setBlockChange(BlockState oldblock, BlockState newBlock, long parent_id) {
        // TODO: Why string? Why?
        this.parent_id = parent_id;
        if (oldblock != null) {
            setOldMaterial(oldblock.getType());
            setOldBlockData(oldblock.getBlockData());

            // TODO: This is using oldblock when it looks like it should use newBlock,
            // but this is how it was when I found it. Test later.
            setMaterial(oldblock.getType());
            setBlockData(oldblock.getBlockData());
        }
    }

    @Override
    public String serialize() {
        return String.valueOf(parent_id);
    }

    @Override
    public void deserialize(String data) {
        parent_id = Long.parseLong(data);
    }

    /**
     * @return the parent_id
     */
    public long getParentId() {
        return parent_id;
    }
}