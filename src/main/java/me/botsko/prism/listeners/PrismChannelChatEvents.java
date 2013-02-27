package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.CommandAction;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.dthielke.herochat.ChannelChatEvent;

public class PrismChannelChatEvents implements Listener {

	
	/**
	 * 
	 */
	private Prism plugin;

	
	/**
	 * 
	 * @param plugin
	 */
	public PrismChannelChatEvents( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onChannelChat(final ChannelChatEvent event){
		if( !plugin.getConfig().getBoolean("prism.tracking.player-chat") ) return;
		String msg = "["+event.getChannel().getNick().toLowerCase() + "] " + event.getMessage();
		Prism.actionsRecorder.addToQueue( new CommandAction(ActionType.PLAYER_CHAT, msg, event.getSender().getPlayer().getLocation(), event.getSender().getName()) );
	}
}