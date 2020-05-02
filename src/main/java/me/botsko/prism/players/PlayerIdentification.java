package me.botsko.prism.players;

import me.botsko.prism.Prism;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.Bukkit;
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
    static String prefix = "prism_";

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
        PrismPlayer prismPlayer = getPrismPlayer(player);
        if (prismPlayer != null) {
            prismPlayer = comparePlayerToCache(player, prismPlayer);
            Prism.debug("Loaded player " + player.getName() + ", id: " + prismPlayer.getId() + " into the cache.");
            Prism.prismPlayers.put(player.getUniqueId(), prismPlayer);
            return prismPlayer;
        }
        prismPlayer = addPlayer(player);
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
    public static PrismPlayer getPrismPlayer(String playerName) {

        Player player = Bukkit.getPlayer(playerName);

        if (player != null) {
            return getPrismPlayer(player);
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
    public static PrismPlayer getPrismPlayer(Player player) {

        if (player.getUniqueId() == null) {
            // If they have a name, we can attempt to find them that way
            if (player.getName() != null && !player.getName().trim().isEmpty()) {
                return getPrismPlayer(player.getName());
            }
            // No name, no UUID, no service.
            return null;
        }

        PrismPlayer prismPlayer;

        // Are they in the cache?
        prismPlayer = Prism.prismPlayers.get(player.getUniqueId());
        if (prismPlayer != null) {
            return prismPlayer;
        }

        // Lookup by UUID
        prismPlayer = lookupByUuid(player.getUniqueId());
        if (prismPlayer != null) {
            return prismPlayer;
        }

        // Still not found, try looking them up by name
        prismPlayer = lookupByName(player.getName());
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

        // Compare for username differences, update database
        if (!player.getName().equals(prismPlayer.getName())) {
            prismPlayer.setName(player.getName());
            updatePlayer(prismPlayer);
        }

        // Compare UUID
        if (!player.getUniqueId().equals(prismPlayer.getUUID())) {
            Prism.log("Player UUID for " + player.getName() + " does not match our cache! " + player.getUniqueId()
                    + " versus cache of " + prismPlayer.getUUID());

            // Update anyway...
            prismPlayer.setUUID(player.getUniqueId());
            updatePlayer(prismPlayer);

        }

        return prismPlayer;

    }

    /**
     * Converts UUID to a string ready for use against database.
     * @param id Uuid
     * @return String
     */
    public static String uuidToDbString(UUID id) {
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
        prefix = Prism.config.getString("prism.mysql.prefix","prism_");
        PrismPlayer prismPlayer = new PrismPlayer(0, player.getUniqueId(), player.getName());
        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s = conn.prepareStatement("INSERT INTO " + prefix
                                + "players (player,player_uuid) VALUES (?,UNHEX(?))",
                        Statement.RETURN_GENERATED_KEYS)
                ) {
            s.setString(1, player.getName());
            s.setString(2, uuidToDbString(player.getUniqueId()));
            s.executeUpdate();

            ResultSet rs = s.getGeneratedKeys();
            if (rs.next()) {
                prismPlayer.setId(rs.getInt(1));
                Prism.debug("Saved and loaded player " + player.getName() + " (" + player.getUniqueId()
                        + ") into the cache.");
                Prism.prismPlayers.put(player.getUniqueId(),
                        new PrismPlayer(rs.getInt(1), player.getUniqueId(), player.getName()));
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
    protected static PrismPlayer addPlayer(String playerName) {
        PrismPlayer fakePlayer = new PrismPlayer(0, UUID.randomUUID(), playerName);
        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s = conn.prepareStatement("INSERT INTO " + prefix
                        + "players (player,player_uuid) VALUES (?,UNHEX(?))", Statement.RETURN_GENERATED_KEYS)
                ) {
            s.setString(1, fakePlayer.getName());
            s.setString(2, uuidToDbString(fakePlayer.getUUID()));
            s.executeUpdate();

            ResultSet rs = s.getGeneratedKeys();
            if (rs.next()) {
                fakePlayer.setId(rs.getInt(1));
                Prism.debug("Saved and loaded fake player " + fakePlayer.getName() + " into the cache.");
                Prism.prismPlayers.put(fakePlayer.getUUID(), fakePlayer);
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
    protected static void updatePlayer(PrismPlayer prismPlayer) {
        prefix = Prism.config.getString("prism.mysql.prefix","prism_");
        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s =  conn.prepareStatement(
                        "UPDATE " + prefix + "players SET player = ?, "
                                + "player_uuid = UNHEX(?) WHERE player_id = ?")
                ) {
            s.setString(1, prismPlayer.getName());
            s.setString(2, uuidToDbString(prismPlayer.getUUID()));
            s.setInt(3, prismPlayer.getId());
            s.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    /**
     * Loads `prism_players` ID for a player into our cache.
     */
    protected static PrismPlayer lookupByName(String playerName) {
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
    protected static PrismPlayer lookupByUuid(UUID uuid) {
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
    public static void cacheOnlinePlayerPrimaryKeys() {

        String[] playerNames;
        playerNames = new String[Bukkit.getServer().getOnlinePlayers().size()];
        int i = 0;
        for (Player pl : Bukkit.getServer().getOnlinePlayers()) {
            playerNames[i] = pl.getName();
            i++;
        }


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