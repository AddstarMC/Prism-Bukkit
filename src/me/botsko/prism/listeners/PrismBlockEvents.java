package me.botsko.prism.listeners;

import java.text.SimpleDateFormat;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.BlockAction;

import org.bukkit.block.Block;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.block.BlockPlaceEvent;

public class PrismBlockEvents implements Listener {
	
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public PrismBlockEvents( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
    public void onBlockBreak(final BlockBreakEvent event){
		
		// Someone cancelled this before we did 
		if ( event.isCancelled() ) {
			return;
		}
		
		Block block = event.getBlock();
		Player player = event.getPlayer();
		
//		// skip if we don't track creative mode
//		if(player.getGameMode() == GameMode.CREATIVE){
//			return;
//		}
		
		java.util.Date date= new java.util.Date();
		String action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		
		plugin.actionsRecorder.addToQueue( 
			new BlockAction(
				action_time,
				"block-break",
				block.getWorld().getName(),
				player.getName(),
				block.getX(),
				block.getY(),
				block.getZ(),
				block
			)
		);
		
	}
	
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
    public void onBlockPlace(final BlockPlaceEvent event){
		
		// Someone cancelled this before we did 
		if ( event.isCancelled() ) {
			return;
		}
		
		Block block = event.getBlock();
		Player player = event.getPlayer();
		
//		// skip if we don't track creative mode
//		if(player.getGameMode() == GameMode.CREATIVE){
//			return;
//		}
		
		java.util.Date date= new java.util.Date();
		String action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		
		plugin.actionsRecorder.addToQueue( 
			new BlockAction(
				action_time,
				"block-place",
				block.getWorld().getName(),
				player.getName(),
				block.getX(),
				block.getY(),
				block.getZ(),
				block
			)
		);
	}
}