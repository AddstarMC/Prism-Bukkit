package me.botsko.prism.players;

import me.botsko.prism.Prism;
import me.botsko.prism.database.sql.SqlPlayerIdentificationBuilder;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public class PlayerIdentification {

    /**
     * Loads `prism_players` ID for a real player into our cache.
     * Runs during PlayerJoin events, so it will never be for a fake/null player.
     *
     * @param uuid Player uuid
     * @param name String
     * @return {@link PrismPlayer}
     */
    public static void cachePrismPlayer(UUID uuid, String name) {
        PrismPlayer prismPlayer;
        prismPlayer = getPrismPlayer(uuid, name);
        if (prismPlayer != null) {
            comparePlayerToCache(name, uuid, prismPlayer);
            Prism.debug("Loaded player " + name + ", id: " + uuid + " into the cache.");
            Prism.prismPlayers.put(uuid, prismPlayer);
            return;
        }
        SqlPlayerIdentificationBuilder.addPlayer(name, uuid);
    }

    /**
     * Gets a player from the cache by name in general this is always an online player but if not it
     * will attempt to get the player id from the database.
     */
    public static PrismPlayer getPrismPlayerByNameFromCache(final String playerName) {

        // Lookup the player
        PrismPlayer prismPlayer = getPrismPlayer(playerName);
        if (prismPlayer != null) {
            // prismPlayer = comparePlayerToCache( player, prismPlayer );
            Prism.debug("Loaded player " + prismPlayer.getName() + ", id: " + prismPlayer.getId() + " into the cache.");
            // Prism.prismPlayers.put( player.getUniqueId(), prismPlayer );
            return prismPlayer;
        }

        // Player is fake, create a record for them
        prismPlayer = SqlPlayerIdentificationBuilder.addFakePlayer(playerName);

        return prismPlayer;

    }

    /**
     * Returns a `prism_players` ID for the described player name. If one cannot be
     * found, returns 0.
     * Used by the recorder in determining proper foreign key
     * - Possibly performs db lookup.,
     *
     * @param playerName String
     * @return PrismPlayer
     */
    private static PrismPlayer getPrismPlayer(String playerName) {

        Player player = Bukkit.getPlayer(playerName);

        if (player != null) {
            return getPrismPlayer(player.getUniqueId(), player.getName());
        }

        // Player not online, we need to go to cache

        return SqlPlayerIdentificationBuilder.lookupByName(playerName);

    }

    /**
     * Returns a `prism_players` ID for the described player object. If one cannot
     * be found, returns 0. - Possibly performs db lookup.,
     * Used by the recorder in determining proper foreign key
     *
     * @return PrismPlayer
     */
    private static @Nullable PrismPlayer getPrismPlayer(UUID uuid, String name) {

        PrismPlayer prismPlayer;
        // Are they in the cache?
        prismPlayer = Prism.prismPlayers.get(uuid);
        if (prismPlayer != null) {
            return prismPlayer;
        }

        // Lookup by UUID
        prismPlayer = SqlPlayerIdentificationBuilder.lookupByUuid(uuid);
        if (prismPlayer != null) {
            if (!prismPlayer.getName().equals(name)) {
                prismPlayer.setName(name);
                SqlPlayerIdentificationBuilder.updatePlayer(prismPlayer);
            }
            return prismPlayer;
        }
        // Still not found, try looking them up by name
        prismPlayer = SqlPlayerIdentificationBuilder.lookupByName(name);
        prismPlayer = comparePlayerToCache(name, uuid, prismPlayer);
        // now check if the uuid is the same as the one logging in ...if it isn't we likely need to
        // create a new player and update the old one with a new name
        return prismPlayer;

    }

    /**
     * Compares the known player to the cached data. If there's a difference we need
     * to handle it.
     * If usernames are different: Update `prism_players` with new name (@todo track
     * historical?)
     * If UUID is different, log an error.
     *
     * @param name        Player name
     * @param uuid        UUID player uuid
     * @param prismPlayer PrismPlayer
     * @return PrismPlayer
     */

    private static @Nullable PrismPlayer comparePlayerToCache(final String name, final UUID uuid, PrismPlayer prismPlayer) {
        if (prismPlayer == null) {
            return null;
        }
        if (!name.equals(prismPlayer.getName())) {
            //ok but now names can be used so lets check if an existing player uses that name
            PrismPlayer test = SqlPlayerIdentificationBuilder.lookupByName(name);
            if (test != null && test.getUuid() != prismPlayer.getUuid()) {
                Prism.warn("Player UUID for " + name + " conflicts with another player: " + test.getUuid()
                        + " we are attempting to update that UUID with a new name before allowing this cache.");
                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(test.getUuid());
                test.setName(offlinePlayer.getName());
                if (test.getName().equals(name)) {
                    Prism.warn("Players appear to have the same name "
                            + "- generally this is impossible with online servers.");
                }
                SqlPlayerIdentificationBuilder.updatePlayer(test);
            }
            prismPlayer.setName(name);
            SqlPlayerIdentificationBuilder.updatePlayer(prismPlayer);
        }
        if (!uuid.equals(prismPlayer.getUuid())) {
            Prism.warn("Player UUID for " + name + " does not match our cache! " + uuid
                    + " versus cache of " + prismPlayer.getName() + " / " + prismPlayer.getUuid());

            // Update anyway...
            prismPlayer.setUuid(uuid);
            SqlPlayerIdentificationBuilder.updatePlayer(prismPlayer);
        }
        return prismPlayer;
    }

}