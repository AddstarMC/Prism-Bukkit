package me.botsko.prism.events;

import org.bukkit.block.BlockState;

public class BlockStateChange {


    private final BlockState originalBlock;
    private final BlockState newBlock;

    /**
     * Constructor.
     * @param originalBlock BlockState
     * @param newBlock BlockState
     */
    public BlockStateChange(BlockState originalBlock, BlockState newBlock) {
        this.originalBlock = originalBlock;
        this.newBlock = newBlock;
    }

    /**
     * Get Original.
     * @return the originalBlock
     */
    public BlockState getOriginalBlock() {
        return originalBlock;
    }

    /**
     * Get new.
     * @return the newBlock
     */
    public BlockState getNewBlock() {
        return newBlock;
    }
}