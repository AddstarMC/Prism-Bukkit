package me.botsko.prism.settings;

import me.botsko.prism.Prism;
import org.bukkit.entity.Player;

public class Settings {

    /**
     * Delete the setting.
     *
     * @param key String
     */
    public static void deleteSetting(String key) {
        deleteSetting(key, null);
    }

    /**
     * Delete the setting.
     * @param key String
     * @param player Player
     */
    public static void deleteSetting(String key, Player player) {
        Prism.getPrismDataSource().createSettingsQuery().deleteSetting(key, player);
    }

    /**
     * Save the setting.
     * @param key String
     * @param value String.
     */
    public static void saveSetting(String key, String value) {
        saveSetting(key, value, null);
    }

    /**
     * Save.
     * @param key String
     * @param value String
     * @param player Player
     */
    public static void saveSetting(String key, String value, Player player) {
        Prism.getPrismDataSource().createSettingsQuery().saveSetting(key, value, player);
    }

    /**
     * Get.
     * @param key String
     * @return String
     */
    public static String getSetting(String key) {
        return getSetting(key, null);
    }

    /**
     * Get.
     * @param key String
     * @param player Player
     * @return String
     */
    public static String getSetting(String key, Player player) {
        return Prism.getPrismDataSource().createSettingsQuery().getSetting(key, player);
    }
}