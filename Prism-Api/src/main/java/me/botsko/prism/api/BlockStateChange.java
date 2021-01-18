package me.botsko.prism.api;

import org.bukkit.block.BlockState;

/**
 * Created for use Prism
 * Created by Narimm on 11/01/2021.
 */
public interface BlockStateChange {

    /**
     * The Original BlockState.
     * @return BlockState
     */
    BlockState getOriginalBlock();

    /**
     * The new blockState.
     * @return BlockState
     */
    BlockState getNewBlock();
}
