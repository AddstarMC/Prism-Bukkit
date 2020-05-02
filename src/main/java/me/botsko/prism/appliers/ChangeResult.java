package me.botsko.prism.appliers;

import me.botsko.prism.events.BlockStateChange;

public class ChangeResult {

    protected final BlockStateChange blockStateChange;
    protected final ChangeResultType changeResultType;

    /**
     * Constructor.
     * @param changeResultType ChangeResultType
     */
    public ChangeResult(ChangeResultType changeResultType) {
        this(changeResultType, null);
    }

    /**
     * Constructor.
     * @param changeResultType ChangeResultType
     * @param blockStateChange BlockStateChange
     */
    public ChangeResult(ChangeResultType changeResultType, BlockStateChange blockStateChange) {
        this.blockStateChange = blockStateChange;
        this.changeResultType = changeResultType;
    }

    /**
     * Get BlockStateChange.
     * @return BlockStateChange
     */
    public BlockStateChange getBlockStateChange() {
        return blockStateChange;
    }

    /**
     * Return ChangeResultType.
     * @return ChangeResultType
     */
    public ChangeResultType getType() {
        return changeResultType;
    }
}