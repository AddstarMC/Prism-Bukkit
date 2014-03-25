package me.botsko.prism.events;

import org.bukkit.block.BlockState;

public class BlockStateChange {

    /**
     * 
     */
    private final BlockState originalBlock;

    /**
     * 
     */
    private final BlockState newBlock;

    /**
     * 
     * @param originalBlock
     * @param newBlock
     */
    public BlockStateChange(BlockState originalBlock, BlockState newBlock) {
        this.originalBlock = originalBlock;
        this.newBlock = newBlock;
    }

    /**
     * @return the originalBlock
     */
    public BlockState getOriginalBlock() {
        return originalBlock;
    }

    /**
     * @return the newBlock
     */
    public BlockState getNewBlock() {
        return newBlock;
    }
}