package me.botsko.prism.database;

import org.bukkit.entity.Player;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 6/04/2019.
 */
public interface SettingsQuery {
    void deleteSetting(String key, Player player);

    void saveSetting(String key, String value, Player player);

    String getSetting(String key, Player player);
}
