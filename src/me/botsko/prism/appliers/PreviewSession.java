package me.botsko.prism.appliers;


import org.bukkit.entity.Player;

public class PreviewSession {

	/**
	 * 
	 */
	protected final Player player;
	
	/**
	 * 
	 */
	protected final Previewable previewer;
	
	/**
	 * 
	 */
	protected final ApplierResult results;
	
	/**
	 * 
	 */
	protected final long queryTime;
	
	
	/**
	 * 
	 * @param player
	 * @param undo
	 * @param args
	 */
	public PreviewSession( Player player, Previewable previewer, ApplierResult results ){
		this.player = player;
		this.previewer = previewer;
		this.results = results;
		java.util.Date date = new java.util.Date();
		this.queryTime = date.getTime();
	}


	/**
	 * @return the player
	 */
	public Player getPlayer() {
		return player;
	}


	/**
	 * @return the previewer
	 */
	public Previewable getPreviewer() {
		return previewer;
	}


	/**
	 * @return the results
	 */
	public ApplierResult getResults() {
		return results;
	}


	/**
	 * @return the queryTime
	 */
	public long getQueryTime() {
		return queryTime;
	}
}