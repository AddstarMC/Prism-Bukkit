package me.botsko.prism.database.sql;

import me.botsko.prism.Prism;
import me.botsko.prism.players.PrismPlayer;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.jetbrains.annotations.Nullable;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.UUID;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 3/01/2021.
 */
public class SqlPlayerIdentificationHelper {

    private static String prefix = Prism.getPrismDataSource().getPrefix();

    /**
     * Loads `prism_players` ID for a player from the database.
     */
    public static PrismPlayer lookupByUuid(UUID uuid) {
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
     * Loads `prism_players` ID for a player performs a db lookup.
     */
    public static @Nullable PrismPlayer lookupByName(String playerName) {
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
     * Converts UUID to a string ready for use against database.
     *
     * @param id Uuid
     * @return String
     */
    private static String uuidToDbString(UUID id) {
        return id.toString().replace("-", "");
    }

    /**
     * Converts UUID to a string ready for use against database.
     *
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
     * @param name Player
     * @param uuid UUID
     * @return PrismPlayer
     */

    public static void addPlayer(final String name, final UUID uuid) {
        prefix = Prism.getPrismDataSource().getPrefix();
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
                        new PrismPlayer(rs.getInt(1), uuid, name));
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
            rs.close();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    /**
     * Saves a fake player's name and generated UUID to the `prism_players` table.
     * At this stage, we're pretty sure the UUID and username do not already exist.
     *
     * @param playerName String
     * @return PrismPlayer
     */
    public static PrismPlayer addFakePlayer(String playerName) {
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
     * Saves a player's UUID to the prism_players table.
     */
    public static void updatePlayer(PrismPlayer prismPlayer) {
        checkAndUpdatePrismPlayer(prismPlayer);
        try (
                Connection conn = Prism.getPrismDataSource().getConnection();
                PreparedStatement s = conn.prepareStatement(
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
     * This method checks a players name does not exist in the database with a different UUID
     * if it does it will then update that player first before then updating the initial player.
     *
     * @param prismPlayer PrismPlayer
     */
    private static void checkAndUpdatePrismPlayer(PrismPlayer prismPlayer) {
        PrismPlayer test = lookupByName(prismPlayer.getName());
        if (test != null && test.getUuid() != prismPlayer.getUuid()) {
            //there is an existing player with that name ...it will need updating this
            OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(test.getUuid());
            if (offlinePlayer.getName() != null) {
                if (offlinePlayer.getName().equals(prismPlayer.getName())) {
                    // 2 players with the same name - this is going to cause major issues.
                    Prism.warn("2 Players exist with the same name Prism cannot load both as per the name.");
                    Prism.warn("Player 1(player to update): " + prismPlayer.getName() + " / " + prismPlayer.getUuid());
                    Prism.warn("Player 2(existing): " + offlinePlayer.getName() + " / " + test.getUuid());
                    Prism.warn("Player 2 will have the name set with a random index.");
                    test.setName(offlinePlayer.getName() + "_" + offlinePlayer.getUniqueId().getMostSignificantBits());
                    updatePlayer(test);
                }
                test.setName(offlinePlayer.getName());
                updatePlayer(test);
            }
        }
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
