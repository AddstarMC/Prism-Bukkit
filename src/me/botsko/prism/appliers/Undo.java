package me.botsko.prism.appliers;

import org.bukkit.block.Block;

public class Undo {
	
	/**
	 * 
	 */
	private Block block;
	
	
	/**
	 * 
	 * @param block
	 */
	public Undo( Block block ){
		this.block = block;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Block getOriginalBlock(){
		return block;
	}
}