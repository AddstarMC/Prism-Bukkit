package me.botsko.prism.appliers;

import java.util.ArrayList;

import me.botsko.prism.events.BlockStateChange;

public class ApplierResult {
	
	/**
	 * 
	 */
	private final int changes_applied;
	
	/**
	 * 
	 */
	private final int changes_skipped;
	
	/**
	 * 
	 */
	private final boolean is_preview;
	
	/**
	 * 
	 */
	private final ArrayList<BlockStateChange> blockStateChanges;
	
	/**
	 * 
	 */
	private final PrismProcessType processType;
	
	
	/**
	 * 
	 * @param changes_applied
	 * @param changes_skipped
	 * @param messages
	 */
	public ApplierResult( boolean is_preview, int changes_applied, int changes_skipped, ArrayList<BlockStateChange> blockStateChanges, PrismProcessType processType ){
		this.changes_applied = changes_applied;
		this.changes_skipped = changes_skipped;
		this.is_preview = is_preview;
		this.blockStateChanges = blockStateChanges;
		this.processType = processType;
	}


	/**
	 * @return the changes_applied
	 */
	public int getChangesApplied() {
		return changes_applied;
	}


	/**
	 * @return the changes_skipped
	 */
	public int getChangesSkipped() {
		return changes_skipped;
	}


	/**
	 * @return the is_preview
	 */
	public boolean isPreview() {
		return is_preview;
	}


	/**
	 * @return the undo
	 */
	public ArrayList<BlockStateChange> getBlockStateChanges(){
		return blockStateChanges;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public PrismProcessType getProcessType(){
		return processType;
	}
}