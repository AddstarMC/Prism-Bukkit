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
	public static String getPlayerKey( Player player, String key ){
		return player.getName() + "." + key;
	}
	
	
	/**
	 * 
	 * @param key
	 */
	public static void deleteSetting( String key ){
		 deleteSetting( key, null );
	}
	
	
	/**
	 * 
	 * @param key
	 */
	public static void deleteSetting( String key, Player player ){
		Connection conn = null;
        PreparedStatement s = null;
		try {
			
			String finalKey = key;
			if( player != null ){
				finalKey = getPlayerKey(player,key);
			}
			
			conn = Prism.dbc();
			s = conn.prepareStatement ("DELETE FROM prism_meta WHERE k = ?");
			s.setString(1, finalKey);
			s.executeUpdate();
	
		} catch (SQLException e) {
//			plugin.logDbError( e );
		} finally {
        	if(s != null) try { s.close(); } catch (SQLException ignored) {}
        	if(conn != null) try { conn.close(); } catch (SQLException ignored) {}
        }
	}
	
	
	/**
	 * 
	 * @param key
	 * @param value
	 * @return
	 */
	public static void saveSetting( String key, String value ){
		saveSetting(key, value, null);
	}
	
	
	/**
	 * 
	 * @param key
	 * @param value
	 * @return
	 */
	public static void saveSetting( String key, String value, Player player ){
		Connection conn = null;
        PreparedStatement s = null;
		try {
			
			String finalKey = key;
			if( player != null ){
				finalKey = getPlayerKey(player,key);
			}
			
			conn = Prism.dbc();
			s = conn.prepareStatement ("DELETE FROM prism_meta WHERE k = ?");
			s.setString(1, finalKey);
			s.executeUpdate();
			
			s = conn.prepareStatement ("INSERT INTO prism_meta (k,v) VALUES (?,?)");
			s.setString(1, finalKey);
			s.setString(2, value);
			s.executeUpdate();
	
		} catch (SQLException e) {
//			plugin.logDbError( e );
		} finally {
        	if(s != null) try { s.close(); } catch (SQLException ignored) {}
        	if(conn != null) try { conn.close(); } catch (SQLException ignored) {}
        }
	}
	
	
	/**
	 * 
	 * @param key
	 * @return
	 */
	public static String getSetting( String key ){
		return getSetting(key,null);
	}
	
	
	/**
	 * 
	 * @param key
	 * @return
	 */
	public static String getSetting( String key, Player player ){
		String value = null;
		Connection conn = null;
		PreparedStatement s = null;
		ResultSet rs = null;
		try {
			
			String finalKey = key;
			if( player != null ){
				finalKey = getPlayerKey(player,key);
			}

			conn = Prism.dbc();
    		s = conn.prepareStatement ("SELECT v FROM prism_meta WHERE k = ? LIMIT 0,1");
    		s.setString(1, finalKey);
    		rs = s.executeQuery();

    		while(rs.next()){
    			value = rs.getString("v");
			}
    		
        } catch (SQLException e) {
//        	plugin.logDbError( e );
        } finally {
        	if(rs != null) try { rs.close(); } catch (SQLException ignored) {}
        	if(s != null) try { s.close(); } catch (SQLException ignored) {}
        	if(conn != null) try { conn.close(); } catch (SQLException ignored) {}
        }
		return value;
	}
}