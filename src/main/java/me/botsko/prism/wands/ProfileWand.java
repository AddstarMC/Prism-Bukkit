package me.botsko.prism.wands;

import me.botsko.prism.Prism;

import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

public class ProfileWand extends WandBase implements Wand{
	
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
	public void playerLeftClick(Player player, Block block) {
		if(block != null){
			showBlockProfile(player, block, block.getLocation());
		}
	}

	
	/**
	 * 
	 */
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
		
		player.sendMessage( plugin.messenger.playerHeaderMsg("Block Profile") );
		
		player.sendMessage( plugin.messenger.playerMsg("Name: " + block.getType().toString().toLowerCase() ) );
		player.sendMessage( plugin.messenger.playerMsg("Alias: " + plugin.getItems().getItemStackAliasById(block.getTypeId(), block.getData()) ) );
		player.sendMessage( plugin.messenger.playerMsg("ID: " + block.getTypeId()+":" + block.getData() ) );
		player.sendMessage( plugin.messenger.playerMsg("Coords: "+block.getX()+" "+block.getY()+" "+block.getZ() ) );
		
	}
	
	
	/**
	 * 
	 */
	public void playerRightClick(Player player, Entity entity) {
		if(entity != null){
			player.sendMessage( plugin.messenger.playerHeaderMsg("Entity Profile") );
			player.sendMessage( plugin.messenger.playerMsg("Name: " + entity.getType().toString().toLowerCase() ) );
			player.sendMessage( plugin.messenger.playerMsg("ID: " + entity.getEntityId() ) );
			player.sendMessage( plugin.messenger.playerMsg("Coords: "+entity.getLocation().getX()+" "+entity.getLocation().getY()+" "+entity.getLocation().getZ() ) );
		}
	}
}