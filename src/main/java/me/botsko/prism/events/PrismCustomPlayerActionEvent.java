package me.botsko.prism.events;

import org.bukkit.ChatColor;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;

public class PrismCustomPlayerActionEvent extends Event {

    private static final HandlerList handlers = new HandlerList();
    private final String pluginName;
    private final String actionTypeName;
    private final Player player;
    private final String message;

    /**
     * Constructor.
     * @param plugin Plugin
     * @param actionTypeName String
     * @param player Player
     * @param message String
     */
    public PrismCustomPlayerActionEvent(Plugin plugin, String actionTypeName, Player player, String message) {
        this.pluginName = plugin.getName();
        this.actionTypeName = actionTypeName;
        this.player = player;
        this.message = message + ChatColor.GOLD + " [" + this.pluginName + "]" + ChatColor.DARK_AQUA;
    }

    /**
     * Get plugin name.
     * @return String
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * Get Action Name.
     * @return String
     */
    public String getActionTypeName() {
        return actionTypeName;
    }

    /**
     * Get the Player.
     * @return the player
     */
    public Player getPlayer() {
        return player;
    }

    /**
     * Get the message.
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * Required by bukkit for proper event handling.
     */

    @NotNull
    @Override
    public HandlerList getHandlers() {
        return handlers;
    }

    /**
     * Required by bukkit for proper event handling.
     */
    @SuppressWarnings("unused")
    public static HandlerList getHandlerList() {
        return handlers;

    }
}