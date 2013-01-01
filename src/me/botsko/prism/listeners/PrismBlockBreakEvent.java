package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.BlockAction;

import org.bukkit.block.Block;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;

public class PrismBlockBreakEvent implements Listener {
	
	private Prism plugin;
	
	/**
	 * 
	 * @param plugin
	 */
	public PrismBlockBreakEvent( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	
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
		
		
		plugin.actionRecorder.addToQueue( 
			new BlockAction(
				"block-break",
				block.getWorld().getName(),
				player.getName(),
				block.getTypeId(),
				block.getData(),
				block.getX(),
				block.getY(),
				block.getZ()
			)
		);
		
	}
}