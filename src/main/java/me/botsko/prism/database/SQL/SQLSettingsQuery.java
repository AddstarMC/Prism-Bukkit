package me.botsko.prism.database.SQL;

import me.botsko.prism.Prism;
import me.botsko.prism.database.AbstractSettingsQuery;
import me.botsko.prism.database.SettingsQuery;
import me.botsko.prism.database.mysql.MySQLPrismDataSource;
import org.bukkit.entity.Player;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 6/04/2019.
 */
public class SQLSettingsQuery extends AbstractSettingsQuery implements SettingsQuery {
    private SQLPrismDataSource dataSource;

    public SQLSettingsQuery(SQLPrismDataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public void deleteSetting(String key, Player player) {
        String prefix = dataSource.getPrefix();
        Connection conn = null;
        PreparedStatement s = null;
        try {

            String finalKey = key;
            if (player != null) {
                finalKey = getPlayerKey(player, key);
            }

            conn = dataSource.getConnection();
            s = conn.prepareStatement("DELETE FROM " + prefix + "meta WHERE k = ?");
            s.setString(1, finalKey);
            s.executeUpdate();

        } catch (final SQLException e) {
            // plugin.logDbError( e );
        } finally {
            if (s != null)
                try {
                    s.close();
                } catch (final SQLException ignored) {
                }
            if (conn != null)
                try {
                    conn.close();
                } catch (final SQLException ignored) {
                }
        }
    }

    @Override
    public void saveSetting(String key, String value, Player player) {
        String prefix = dataSource.getPrefix();
        Connection conn = null;
        PreparedStatement s = null;
        PreparedStatement s2 = null;
        try {

            String finalKey = key;
            if (player != null) {
                finalKey = getPlayerKey(player, key);
            }

            conn = dataSource.getConnection();
            s = conn.prepareStatement("DELETE FROM " + prefix + "meta WHERE k = ?");
            s.setString(1, finalKey);
            s.executeUpdate();

            s2 = conn.prepareStatement("INSERT INTO " + prefix + "meta (k,v) VALUES (?,?)");
            s2.setString(1, finalKey);
            s2.setString(2, value);
            s2.executeUpdate();

        } catch (final SQLException e) {
            // plugin.logDbError( e );
        } finally {
            if (s != null)
                try {
                    s.close();
                } catch (final SQLException ignored) {
                }
            if (s2 != null)
                try {
                    s2.close();
                } catch (final SQLException ignored) {
                }
            if (conn != null)
                try {
                    conn.close();
                } catch (final SQLException ignored) {
                }
        }
    }

    @Override
    public String getSetting(String key, Player player) {
        String prefix = dataSource.getPrefix();
        String value = null;
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            String finalKey = key;
            if (player != null) {
                finalKey = getPlayerKey(player, key);
            }

            conn = dataSource.getConnection();
            s = conn.prepareStatement("SELECT v FROM " + prefix + "meta WHERE k = ? LIMIT 0,1");
            s.setString(1, finalKey);
            rs = s.executeQuery();

            while (rs.next()) {
                value = rs.getString("v");
            }

        } catch (final SQLException e) {
            // plugin.logDbError( e );
        } finally {
            if (rs != null)
                try {
                    rs.close();
                } catch (final SQLException ignored) {
                }
            if (s != null)
                try {
                    s.close();
                } catch (final SQLException ignored) {
                }
            if (conn != null)
                try {
                    conn.close();
                } catch (final SQLException ignored) {
                }
        }
        return value;
    }
}
