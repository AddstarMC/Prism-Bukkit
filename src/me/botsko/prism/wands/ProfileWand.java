package me.botsko.prism.wands;

import me.botsko.prism.Prism;

import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

public class ProfileWand implements Wand {
	
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public ProfileWand( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 */
	@Override
	public void playerLeftClick(Player player, Block block) {
		if(block != null){
			showBlockProfile(player, block, block.getLocation());
		}
	}

	
	/**
	 * 
	 */
	@Override
	public void playerRightClick(Player player, Block block) {
		if(block != null){
			showBlockProfile(player, block, block.getLocation());
		}
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 * @param loc
	 */
	protected void showBlockProfile( Player player, Block block, Location loc ){
		
		player.sendMessage( plugin.playerHeaderMsg("Block Profile") );
		
		player.sendMessage( plugin.playerMsg("Name: " + block.getType().toString().toLowerCase() ) );
		player.sendMessage( plugin.playerMsg("Alias: " + plugin.getItems().getItemStackAliasById(block.getTypeId(), block.getData()) ) );
		player.sendMessage( plugin.playerMsg("ID: " + block.getTypeId()+":" + block.getData() ) );
		player.sendMessage( plugin.playerMsg("Coords: "+block.getX()+" "+block.getY()+" "+block.getZ() ) );
		
	}
	
	
	/**
	 * 
	 */
	@Override
	public void playerRightClick(Player player, Entity entity) {
		if(entity != null){
			player.sendMessage( plugin.playerHeaderMsg("Entity Profile") );
			player.sendMessage( plugin.playerMsg("Name: " + entity.getType().toString().toLowerCase() ) );
			player.sendMessage( plugin.playerMsg("ID: " + entity.getEntityId() ) );
			player.sendMessage( plugin.playerMsg("Coords: "+entity.getLocation().getX()+" "+entity.getLocation().getY()+" "+entity.getLocation().getZ() ) );
		}
	}
}