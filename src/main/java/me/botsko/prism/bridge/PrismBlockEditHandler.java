package me.botsko.prism.bridge;

import com.sk89q.worldedit.event.extent.EditSessionEvent;
import com.sk89q.worldedit.extension.platform.Actor;
import com.sk89q.worldedit.util.eventbus.Subscribe;

public class PrismBlockEditHandler {
	@Subscribe
	public void wrapForLogging(EditSessionEvent event) {
		Actor actor = event.getActor();
		if (actor != null && actor.isPlayer()) {
			event.setExtent(new PrismWorldEditLogger(actor, event.getExtent(), event.getWorld()));
		}
	}
}