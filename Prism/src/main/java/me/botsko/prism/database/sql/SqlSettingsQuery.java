package me.botsko.prism.database.sql;

import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.database.AbstractSettingsQuery;
import me.botsko.prism.database.SettingsQuery;
import org.bukkit.entity.Player;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 6/04/2019.
 */
public class SqlSettingsQuery extends AbstractSettingsQuery implements SettingsQuery {
    private final SqlPrismDataSource dataSource;
    protected static String prefix = "prism_";

    public SqlSettingsQuery(SqlPrismDataSource dataSource) {
        this.dataSource = dataSource;
        prefix = dataSource.getPrefix();
    }

    @Override
    public void deleteSetting(String key, Player player) {
        try (
                Connection conn =  dataSource.getConnection();
                PreparedStatement s = conn.prepareStatement("DELETE FROM " + prefix + "meta WHERE k = ?")
        ) {
            String finalKey = key;
            if (player != null) {
                finalKey = getPlayerKey(player, key);
            }
            s.setString(1, finalKey);
            s.executeUpdate();

        } catch (final SQLException e) {
            PrismLogHandler.debug("Database Error:" + e.getMessage());
        }
    }

    @Override
    public void saveSetting(String key, String value, Player player) {

        String finalKey = key;
        if (player != null) {
            finalKey = getPlayerKey(player, key);
        }
        try (
                Connection conn = dataSource.getConnection();
                PreparedStatement s = conn.prepareStatement("DELETE FROM " + prefix + "meta WHERE k = ?");
                PreparedStatement s2 = conn.prepareStatement(getInsertQuery())
                ) {
            s.setString(1, finalKey);
            s.executeUpdate();
            s2.setString(1, finalKey);
            s2.setString(2, value);
            s2.executeUpdate();
        } catch (final SQLException e) {
            PrismLogHandler.debug("Database Error:" + e.getMessage());
        }
    }

    @Override
    public String getSetting(String key, Player player) {
        String value = null;
        ResultSet rs;


        String finalKey = key;
        if (player != null) {
            finalKey = getPlayerKey(player, key);
        }
        try (
                Connection conn = dataSource.getConnection();
                PreparedStatement s =
                        conn.prepareStatement(getSelectQuery())
        ) {
            s.setString(1, finalKey);
            rs = s.executeQuery();

            while (rs.next()) {
                value = rs.getString("v");
            }

        } catch (final SQLException e) {
            PrismLogHandler.debug("Database Error:" + e.getMessage());
        }
        return value;
    }

    protected String getSelectQuery() {
        return "SELECT v FROM " + prefix + "meta WHERE k = ? FETCH FIRST 1 row only;";
    }
    protected String getInsertQuery() {
        return "INSERT INTO " + prefix + "meta (id, k , v) VALUES (DEFAULT,?,?)";
    }
}
