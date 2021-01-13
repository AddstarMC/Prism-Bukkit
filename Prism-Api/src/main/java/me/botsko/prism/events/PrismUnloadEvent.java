package me.botsko.prism.events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * The Prism Unload Event is called when Prism has completely unloaded.
 *
 * @author Narimm
 */
public class PrismUnloadEvent extends Event {

    private static final HandlerList handlers = new HandlerList();

    protected PrismUnloadEvent() {
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }
}
