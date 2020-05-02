package me.botsko.prism.actionlibs;

import me.botsko.prism.Prism;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.GameMode;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;

import java.util.List;

public class Ignore {

    private final Prism plugin;
    private final List<String> ignorePlayers;
    private final boolean ignorePlayersWhiteList;
    private final List<String> ignoreWorlds;
    private final boolean ignoreWorldsWhiteList;
    private final boolean ignoreCreative;

    /**
     * Create Ignore.
     *
     * @param plugin the plugin.
     */
    public Ignore(Prism plugin) {
        this.plugin = plugin;
        ignorePlayers = plugin.getConfig().getStringList("prism.ignore.players");
        ignorePlayersWhiteList = plugin.getConfig().getBoolean("prism.ignore.players_whitelist");
        ignoreWorlds = plugin.getConfig().getStringList("prism.ignore.worlds");
        ignoreWorldsWhiteList = plugin.getConfig().getBoolean("prism.ignore.worlds_whitelist");
        ignoreCreative = plugin.getConfig().getBoolean("prism.ignore.players-in-creative");
    }

    /**
     * Check event typ.
     *
     * @param actionTypeName type.
     * @return boolean
     */
    public boolean event(String actionTypeName) {

        // Always track Prism actions - it's mainly internal
        if (actionTypeName.contains("prism")) {
            return true;
        }

        if (TypeUtils.subStrOccurences(actionTypeName, "-") != 1
                || plugin.getConfig().getBoolean("prism.tracking." + actionTypeName)) {
            return true;
        } else {
            Prism.debug("Ignoring Action Type: " + actionTypeName);
            return false;
        }
    }

    /**
     * Check whether an action should be ignored.
     *
     * @param actionTypeName type to ignore.
     * @param world          world to check.
     * @param player         player to check
     * @return boolean.
     */
    public boolean event(String actionTypeName, World world, String player) {
        return event(actionTypeName, world) && event(player);
    }

    /**
     * Check whether an action should be ignored.
     *
     * @param actionTypeName type to ignore.
     * @param player         player to check.
     * @return boolean.
     */
    public boolean event(String actionTypeName, Player player) {

        if (!event(actionTypeName, player.getWorld())) {
            return false;
        }

        // Does the player have perms to ignore this action type?
        if (plugin.getConfig().getBoolean("prism.ignore.enable-perm-nodes")
                && player.hasPermission("prism.ignore.tracking." + actionTypeName)) {
            Prism.debug("Player has permission node to ignore " + actionTypeName);
            return false;
        }

        return event(player);

    }

    /**
     * Check if we are ignoring a player.
     *
     * @param player the player
     * @return boolean
     */
    public boolean event(Player player) {

        if (player == null) {
            Prism.debug("Player is null will be ignored");
            return false;
        }

        // Should we ignore this player?
        if (ignorePlayers != null && ignorePlayers.contains(player.getName()) != ignorePlayersWhiteList) {
            Prism.debug("Player is being ignored, per config: " + player.getName());
            return false;
        }

        // Should we ignore this player for being in creative?
        if (ignoreCreative) {
            if (player.getGameMode().equals(GameMode.CREATIVE)) {
                Prism.debug("Player is in creative mode, creative mode ignored: " + player.getName());
                return false;
            }
        }

        return true;
    }

    /**
     * Check whether an action should be ignored.
     *
     * @param actionTypeName type to ignore.
     * @param block          to check.
     * @return boolean.
     */
    public boolean event(String actionTypeName, Block block) {
        return event(actionTypeName, block.getWorld());
    }

    /**
     * Check whether an action should be ignored.
     *
     * @param actionTypeName type to ignore.
     * @param world          world to check.
     * @return boolean.
     */
    public boolean event(String actionTypeName, World world) {

        // Should we ignore this world?
        if (ignoreWorlds != null && ignoreWorlds.contains(world.getName()) != ignoreWorldsWhiteList) {
            Prism.debug("World is being ignored, per config: " + world.getName());
            return false;
        }

        return event(actionTypeName);

    }
}