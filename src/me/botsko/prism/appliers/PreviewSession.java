package me.botsko.prism.appliers;

import java.util.ArrayList;

import org.bukkit.entity.Player;

public class PreviewSession {
	
	/**
	 * 
	 */
	protected ArrayList<Undo> undo_queue = new ArrayList<Undo>();
	
	/**
	 * 
	 */
	protected Player player;
	
	/**
	 * 
	 */
	protected String[] args;
	
	
	/**
	 * 
	 * @param player
	 * @param undo
	 * @param args
	 */
	public PreviewSession( Player player, ArrayList<Undo> undo, String[] args ){
		this.undo_queue = undo;
		this.player = player;
		this.args = args;
	}


	/**
	 * @return the undo_queue
	 */
	public ArrayList<Undo> getUndo_queue() {
		return undo_queue;
	}


	/**
	 * @return the player
	 */
	public Player getPlayer() {
		return player;
	}


	/**
	 * @return the args
	 */
	public String[] getArgs() {
		return args;
	}
}