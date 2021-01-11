package me.botsko.prism.events;

import me.botsko.prism.api.BlockStateChange;
import org.bukkit.block.BlockState;

public class BlockStateChangeImpl implements BlockStateChange {


    private final BlockState originalBlock;
    private final BlockState newBlock;

    /**
     * Constructor.
     * @param originalBlock BlockState
     * @param newBlock BlockState
     */
    public BlockStateChangeImpl(BlockState originalBlock, BlockState newBlock) {
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