package me.botsko.prism.settings;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.bukkit.entity.Player;

import me.botsko.prism.Prism;

public class Settings {

    /**
     * 
     * @param player
     * @param key
     * @return
     */
    public static String getPlayerKey(Player player, String key) {
        return player.getName() + "." + key;
    }

    /**
     * 
     * @param key
     */
    public static void deleteSetting(String key) {
        deleteSetting( key, null );
    }

    /**
     * 
     * @param key
     */
    public static void deleteSetting(String key, Player player) {
        String prefix = Prism.config.getString("prism.mysql.prefix");
        Connection conn = null;
        PreparedStatement s = null;
        try {

            String finalKey = key;
            if( player != null ) {
                finalKey = getPlayerKey( player, key );
            }

            conn = Prism.dbc();
            s = conn.prepareStatement( "DELETE FROM " + prefix + "meta WHERE k = ?" );
            s.setString( 1, finalKey );
            s.executeUpdate();

        } catch ( final SQLException e ) {
            // plugin.logDbError( e );
        } finally {
            if( s != null )
                try {
                    s.close();
                } catch ( final SQLException ignored ) {}
            if( conn != null )
                try {
                    conn.close();
                } catch ( final SQLException ignored ) {}
        }
    }

    /**
     * 
     * @param key
     * @param value
     * @return
     */
    public static void saveSetting(String key, String value) {
        saveSetting( key, value, null );
    }

    /**
     * 
     * @param key
     * @param value
     * @return
     */
    public static void saveSetting(String key, String value, Player player) {
        String prefix = Prism.config.getString("prism.mysql.prefix");
        Connection conn = null;
        PreparedStatement s = null;
        try {

            String finalKey = key;
            if( player != null ) {
                finalKey = getPlayerKey( player, key );
            }

            conn = Prism.dbc();
            s = conn.prepareStatement( "DELETE FROM " + prefix + "meta WHERE k = ?" );
            s.setString( 1, finalKey );
            s.executeUpdate();

            s = conn.prepareStatement( "INSERT INTO " + prefix + "meta (k,v) VALUES (?,?)" );
            s.setString( 1, finalKey );
            s.setString( 2, value );
            s.executeUpdate();

        } catch ( final SQLException e ) {
            // plugin.logDbError( e );
        } finally {
            if( s != null )
                try {
                    s.close();
                } catch ( final SQLException ignored ) {}
            if( conn != null )
                try {
                    conn.close();
                } catch ( final SQLException ignored ) {}
        }
    }

    /**
     * 
     * @param key
     * @return
     */
    public static String getSetting(String key) {
        return getSetting( key, null );
    }

    /**
     * 
     * @param key
     * @return
     */
    public static String getSetting(String key, Player player) {
        String prefix = Prism.config.getString("prism.mysql.prefix");
        String value = null;
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            String finalKey = key;
            if( player != null ) {
                finalKey = getPlayerKey( player, key );
            }

            conn = Prism.dbc();
            s = conn.prepareStatement( "SELECT v FROM " + prefix + "meta WHERE k = ? LIMIT 0,1" );
            s.setString( 1, finalKey );
            rs = s.executeQuery();

            while ( rs.next() ) {
                value = rs.getString( "v" );
            }

        } catch ( final SQLException e ) {
            // plugin.logDbError( e );
        } finally {
            if( rs != null )
                try {
                    rs.close();
                } catch ( final SQLException ignored ) {}
            if( s != null )
                try {
                    s.close();
                } catch ( final SQLException ignored ) {}
            if( conn != null )
                try {
                    conn.close();
                } catch ( final SQLException ignored ) {}
        }
        return value;
    }
}