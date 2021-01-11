package me.botsko.prism.appliers;

import me.botsko.prism.api.BlockStateChange;
import me.botsko.prism.api.ChangeResult;
import me.botsko.prism.api.ChangeResultType;

public class ChangeResultImpl implements ChangeResult {

    protected final BlockStateChange blockStateChange;
    protected final ChangeResultType changeResultType;

    /**
     * Constructor.
     * @param changeResultType ChangeResultType
     */
    public ChangeResultImpl(ChangeResultType changeResultType) {
        this(changeResultType, null);
    }

    /**
     * Constructor.
     * @param changeResultType ChangeResultType
     * @param blockStateChange BlockStateChange
     */
    public ChangeResultImpl(ChangeResultType changeResultType, BlockStateChange blockStateChange) {
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