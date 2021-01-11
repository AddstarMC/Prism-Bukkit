package me.botsko.prism.api.events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 10/01/2021.
 */
public class PrismUnloadEvent extends Event {

    private static final HandlerList handlers = new HandlerList();

    public PrismUnloadEvent() {
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }
}
