package me.botsko.prism.appliers;

import me.botsko.prism.events.BlockStateChange;

public class ChangeResult {

    /**
	 * 
	 */
    protected final BlockStateChange blockStateChange;

    /**
	 * 
	 */
    protected final ChangeResultType changeResultType;

    /**
     * 
     * @param changeResultType
     */
    public ChangeResult(ChangeResultType changeResultType) {
        this( changeResultType, null );
    }

    /**
     * 
     * @param changeResultType
     * @param blockStateChange
     */
    public ChangeResult(ChangeResultType changeResultType, BlockStateChange blockStateChange) {
        this.blockStateChange = blockStateChange;
        this.changeResultType = changeResultType;
    }

    /**
     * 
     * @return
     */
    public BlockStateChange getBlockStateChange() {
        return blockStateChange;
    }

    /**
     * 
     * @return
     */
    public ChangeResultType getType() {
        return changeResultType;
    }
}