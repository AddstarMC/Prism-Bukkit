package me.botsko.prism.appliers;

import me.botsko.prism.events.containers.BlockStateChange;

public class BlockChangeResult {
	
	/**
	 * 
	 */
	protected ChangeResultType changeResultType;
	
	/**
	 * 
	 */
	protected BlockStateChange blockStateChange;
	
	
	/**
	 * 
	 * @param changeResultType
	 * @param blockStateChange
	 */
	public BlockChangeResult( ChangeResultType changeResultType, BlockStateChange blockStateChange ){
		this.changeResultType = changeResultType;
		this.blockStateChange = blockStateChange;
	}


	/**
	 * @return the changeResultType
	 */
	public ChangeResultType getChangeResultType() {
		return changeResultType;
	}


	/**
	 * @return the blockStateChange
	 */
	public BlockStateChange getBlockStateChange() {
		return blockStateChange;
	}
}