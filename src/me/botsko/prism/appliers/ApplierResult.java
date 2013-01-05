package me.botsko.prism.appliers;

import java.util.ArrayList;

public class ApplierResult {
	
	/**
	 * 
	 */
	protected int changes_applied = 0;
	
	/**
	 * 
	 */
	protected int changes_skipped = 0;
	
	/**
	 * 
	 */
	protected ArrayList<String> messages = new ArrayList<String>();
	
	/**
	 * 
	 */
	protected boolean is_preview;
	
	/**
	 * 
	 */
	protected ArrayList<Undo> undo = new ArrayList<Undo>();
	
	
	/**
	 * 
	 * @param changes_applied
	 * @param changes_skipped
	 * @param messages
	 */
	public ApplierResult( boolean is_preview, int changes_applied, int changes_skipped, ArrayList<Undo> undo, ArrayList<String> messages ){
		this.changes_applied = changes_applied;
		this.changes_skipped = changes_skipped;
		this.messages = messages;
		this.is_preview = is_preview;
		this.undo = undo;
	}


	/**
	 * @return the changes_applied
	 */
	public int getChanges_applied() {
		return changes_applied;
	}


	/**
	 * @return the changes_skipped
	 */
	public int getChanges_skipped() {
		return changes_skipped;
	}


	/**
	 * @return the messages
	 */
	public ArrayList<String> getMessages() {
		return messages;
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
	public ArrayList<Undo> getUndoQueue(){
		return undo;
	}
}