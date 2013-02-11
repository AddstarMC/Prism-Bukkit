package me.botsko.prism.wands;

import org.bukkit.block.Block;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

public interface Wand {
	
	
	/**
	 * 
	 */
	public void playerLeftClick( Player player, Block block );
	
	
	/**
	 * 
	 */
	public void playerRightClick( Player player, Block block );


	/**
	 * 
	 * @param player
	 * @param entity
	 */
	public void playerRightClick(Player player, Entity entity);

}