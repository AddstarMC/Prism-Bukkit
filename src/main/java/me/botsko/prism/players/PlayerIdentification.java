package me.botsko.prism.players;

import me.botsko.prism.Prism;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.UUID;

public class PlayerIdentification {
    /**
     * This assists with code completion for SQL statements.
     */
    @SuppressWarnings("UnusedAssignment")
    private static String prefix = "prism_";

    static {
        prefix = Prism.config.getString("prism.mysql.prefix","prism_");
    }

    /**
     * Loads `prism_players` ID for a real player into our cache.
     * Runs during PlayerJoin events, so it will never be for a fake/null player.
     *
     * @param player Player
     */
    public static PrismPlayer cachePrismPlayer(@NotNull final Player player) {
        // Lookup the player
        return cachePrismPlayer(player.getUniqueId(),player.getName());
    }

    /**
     * Loads `prism_players` ID for a real player into our cache.
     * Runs during PlayerJoin events, so it will never be for a fake/null player.
     *
     * @param uuid Player uuid
     * @param name String
     * @return {@link PrismPlayer}
     */
    public static PrismPlayer cachePrismPlayer(UUID uuid,String name) {
        PrismPlayer prismPlayer;
        prismPlayer = getPrismPlayer(uuid,name);
        if (prismPlayer != null) {
            PrismPlayer checkPrismPlayer = comparePlayerToCache(name,uuid, prismPlayer);
            Prism.debug("Loaded player " + name + ", id: " + uuid + " into the cache.");
            Prism.prismPlayers.put(uuid, prismPlayer);
            return checkPrismPlayer;
        }
        prismPlayer = addPlayer(name,uuid);
        return prismPlayer;
    }

    /**
     * Loads `prism_players` ID for a real player into our cache.
     * Runs during PlayerJoin events, so it will never be for a fake/null player.
     */
    public static PrismPlayer cachePrismPlayer(final String playerName) {

        // Lookup the player
        PrismPlayer prismPlayer = getPrismPlayer(playerName);
        if (prismPlayer != null) {
            // prismPlayer = comparePlayerToCache( player, prismPlayer );
            Prism.debug("Loaded player " + prismPlayer.getName() + ", id: " + prismPlayer.getId() + " into the cache.");
            // Prism.prismPlayers.put( player.getUniqueId(), prismPlayer );
            return prismPlayer;
        }

        // Player is new, create a record for them
        prismPlayer = addPlayer(playerName);

        return prismPlayer;

    }

    /**
     * Returns a `prism_players` ID for the described player name. If one cannot be
     * found, returns 0.
     * Used by the recorder in determining proper foreign key
     * - Possibly performs db lookup.,
     * @param playerName String
     * @return PrismPlayer
     */
    private static PrismPlayer getPrismPlayer(String playerName) {

        Player player = Bukkit.getPlayer(playerName);

        if (player != null) {
            return getPrismPlayer(player.getUniqueId(),player.getName());
        }

        // Player not online, we need to go to cache

        return lookupByName(playerName);

    }

    /**
     * Returns a `prism_players` ID for the described player object. If one cannot
     * be found, returns 0. - Possibly performs db lookup.,
     * Used by the recorder in determining proper foreign key
     *
     * @return PrismPlayer
     */
    private static PrismPlayer getPrismPlayer(UUID uuid, String name) {

        PrismPlayer prismPlayer;
        // Are they in the cache?
        prismPlayer = Prism.prismPlayers.get(uuid);
        if (prismPlayer != null) {
            return prismPlayer;
        }

        // Lookup by UUID
        prismPlayer = lookupByUuid(uuid);
        // does the name match
        if (prismPlayer != null) {
            return prismPlayer;
        }

        // Still not found, try looking them up by name
        prismPlayer = lookupByName(name);
        // now check if the uuid is the same as the one logging in ...if it isnt we likely need to
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
     * @param player Player
     * @param prismPlayer PrismPlayer
     * @return PrismPlayer
     */
    protected static PrismPlayer comparePlayerToCache(Player player, PrismPlayer prismPlayer) {
        return comparePlayerToCache(player.getName(),player.getUniqueId(),prismPlayer);
    }

    private static PrismPlayer comparePlayerToCache(final String name, final UUID uuid, PrismPlayer prismPlayer) {
        if (!name.equals(prismPlayer.getName())) {
            //ok but now names can be used so lets check if an existing player uses that name
            PrismPlayer test = lookupByName(name);
            if (test != null) {
                Prism.warn("Player UUID for " + name + " conflicts with another player: " + test.getUuid()
                        + " we are attempting to update the that UUID with a new name before allowing this cache.");
                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(test.getUuid());
                test.setName(offlinePlayer.getName());
                if (test.getName().equals(name)){
                    Prism.warn("Players appear to have the same name - generally this is impossible with online servers.");
                }
                updatePlayer(test);
            }
            prismPlayer.setName(name);
            updatePlayer(prismPlayer);
        }
        if (!uuid.equals(prismPlayer.getUuid())) {
            Prism.warn("Player UUID for " + name + " does not match our cache! " + uuid
                    + " versus cache of " + prismPlayer.getUuid());

            // Update anyway...
            prismPlayer.setUuid(uuid);
            updatePlayer(prismPlayer);

        }
        return prismPlayer;
    }

    /**
     * Converts UUID to a string ready for use against database.
     * @param id Uuid
     * @return String
     */
    private static String uuidToDbString(UUID id) {
        return id.toString().replace("-", "");
    }

    /**
     * Converts UUID to a string ready for use against database.
     * @param uuid string
     * @return Uuid
     */
    public static UUID uuidFromDbString(String uuid) {
        // Positions need to be -2
        String completeUuid = uuid.substring(0, 8);
        completeUuid += "-" + uuid.substring(8, 12);
        completeUuid += "-" + uuid.substring(12, 16);
        completeUuid += "-" + uuid.substring(16, 20);
        completeUuid += "-" + uuid.substring(20);
        completeUuid = completeUuid.toLowerCase();
        return UUID.fromString(completeUuid);
    }

    /**
     * Saves a real player's UUID and current Username to the `prism_players` table.
     * At this stage, we're pretty sure the UUID and username do not already exist.
     *
     * @param player Player
     * @return PrismPlayer
     */
    protected static PrismPlayer addPlayer(Player player) {
        return addPlayer(player.getName(),player.getUniqueId());
    }

    private static PrismPlayer addPlayer(final String name, final UUID uuid) {
        prefix = Prism.config.getString("prism.mysql.prefix","prism_");
        PrismPlayer prismPlayer = new PrismPlayer(0, uuid, name);
        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s = conn.prepareStatement("INSERT INTO " + prefix
                                + "players (player,player_uuid) VALUES (?,UNHEX(?))",
                        Statement.RETURN_GENERATED_KEYS)
        ) {
            s.setString(1, name);
            s.setString(2, uuidToDbString(uuid));
            s.executeUpdate();

            ResultSet rs = s.getGeneratedKeys();
            if (rs.next()) {
                prismPlayer.setId(rs.getInt(1));
                Prism.debug("Saved and loaded player " + name + " (" + uuid
                        + ") into the cache.");
                Prism.prismPlayers.put(uuid,
                        new PrismPlayer(rs.getInt(1),uuid, name));
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
            rs.close();
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return prismPlayer;
    }

    /**
     * Saves a fake player's name and generated UUID to the `prism_players` table.
     * At this stage, we're pretty sure the UUID and username do not already exist.
     *
     * @param playerName String
     * @return PrismPlayer
     */
    private static PrismPlayer addPlayer(String playerName) {
        PrismPlayer fakePlayer = new PrismPlayer(0, UUID.randomUUID(), playerName);
        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s = conn.prepareStatement("INSERT INTO " + prefix
                        + "players (player,player_uuid) VALUES (?,UNHEX(?))", Statement.RETURN_GENERATED_KEYS)
                ) {
            s.setString(1, fakePlayer.getName());
            s.setString(2, uuidToDbString(fakePlayer.getUuid()));
            s.executeUpdate();

            ResultSet rs = s.getGeneratedKeys();
            if (rs.next()) {
                fakePlayer.setId(rs.getInt(1));
                Prism.debug("Saved and loaded fake player " + fakePlayer.getName() + " into the cache.");
                Prism.prismPlayers.put(fakePlayer.getUuid(), fakePlayer);
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
            rs.close();
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return fakePlayer;
    }

    /**
     * Saves a player's UUID to the prism_players table. We cache the current
     * username as well.
     */
    private static void updatePlayer(PrismPlayer prismPlayer) {
        prefix = Prism.config.getString("prism.mysql.prefix","prism_");
        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s =  conn.prepareStatement(
                        "UPDATE " + prefix + "players SET player = ?, "
                                + "player_uuid = UNHEX(?) WHERE player_id = ?")
                ) {
            s.setString(1, prismPlayer.getName());
            s.setString(2, uuidToDbString(prismPlayer.getUuid()));
            s.setInt(3, prismPlayer.getId());
            s.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    /**
     * Loads `prism_players` ID for a player into our cache.
     */
    private static PrismPlayer lookupByName(String playerName) {
        PrismPlayer prismPlayer = null;

        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s = conn.prepareStatement(
                        "SELECT player_id, player, HEX(player_uuid) FROM " + prefix + "players WHERE player = ?")

                ) {
            s.setString(1, playerName);
            ResultSet rs = s.executeQuery();
            if (rs.next()) {
                prismPlayer = new PrismPlayer(rs.getInt(1), uuidFromDbString(rs.getString(3)), rs.getString(2));
            }
            rs.close();
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return prismPlayer;
    }

    /**
     * Loads `prism_players` ID for a player into our cache.
     */
    private static PrismPlayer lookupByUuid(UUID uuid) {
        PrismPlayer prismPlayer = null;
        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s = conn.prepareStatement("SELECT player_id, player, HEX(player_uuid) FROM " + prefix
                        + "players WHERE player_uuid = UNHEX(?)")
                ) {
            s.setString(1, uuidToDbString(uuid));
            ResultSet rs = s.executeQuery();
            if (rs.next()) {
                prismPlayer = new PrismPlayer(rs.getInt(1), uuidFromDbString(rs.getString(3)), rs.getString(2));
            }
            rs.close();
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return prismPlayer;
    }

    /**
     * Build-load all online players into cache.
     */
    public static void cacheOnlinePlayerPrimaryKeys(String[] playerNames) {
        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s = conn.prepareStatement(
                        "SELECT player_id, player, HEX(player_uuid) FROM " + prefix + "players WHERE player IN (?)")
                ) {
            s.setString(1, "'" + TypeUtils.join(playerNames, "','") + "'");
            ResultSet rs = s.executeQuery();
            while (rs.next()) {
                PrismPlayer prismPlayer = new PrismPlayer(rs.getInt(1), uuidFromDbString(rs.getString(3)),
                        rs.getString(2));
                Prism.debug("Loaded player " + rs.getString(2) + ", id: " + rs.getInt(1) + " into the cache.");
                Prism.prismPlayers.put(UUID.fromString(rs.getString(2)), prismPlayer);
            }
            rs.close();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
}