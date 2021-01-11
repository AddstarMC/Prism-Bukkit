package me.botsko.prism.api.events;

import me.botsko.prism.api.PrismApi;
import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 10/01/2021.
 */
public class PrismLoadedEvent extends Event {

    private final static HandlerList handlers = new HandlerList();
    private PrismApi api;
    public PrismLoadedEvent(PrismApi api) {
        super(true);
        this.api = api;
    }

    public PrismApi getApi() {
        return api;
    }

    @NotNull
    @Override
    public HandlerList getHandlers() {
        return handlers;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }
}
