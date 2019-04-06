package me.botsko.prism.database;

import org.bukkit.entity.Player;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 6/04/2019.
 */
public abstract class AbstractSettingsQuery implements SettingsQuery {
    /**
     *
     * @param player
     * @param key
     * @return
     */
    public String getPlayerKey(Player player, String key) {
        return player.getName() + "." + key;
    }
}
