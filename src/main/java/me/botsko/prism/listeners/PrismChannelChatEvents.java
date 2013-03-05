package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.dthielke.herochat.ChannelChatEvent;

public class PrismChannelChatEvents implements Listener {
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onChannelChat(final ChannelChatEvent event){
		if( !Prism.getIgnore().event("player-chat", event.getSender().getPlayer()) ) return;
		String msg = "["+event.getChannel().getNick().toLowerCase() + "] " + event.getMessage();
		Prism.actionsRecorder.addToQueue( ActionFactory.create("player-chat", event.getSender().getPlayer(), msg) );
	}
}