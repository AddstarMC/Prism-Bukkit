package me.botsko.prism.database;

import org.bukkit.entity.Player;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 6/04/2019.
 */
public abstract class AbstractSettingsQuery implements SettingsQuery {
    /**
     * Get the Player key name.
     * @param player the Player
     * @param key    the setting to return
     * @return String
     */
    public String getPlayerKey(Player player, String key) {
        return player.getName() + "." + key;
    }
}
