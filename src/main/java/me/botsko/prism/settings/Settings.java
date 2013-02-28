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
		Connection conn = Prism.dbc();
        PreparedStatement s;
		try {
			
			String finalKey = key;
			if( player != null ){
				finalKey = getPlayerKey(player,key);
			}
			
			s = conn.prepareStatement ("DELETE FROM prism_meta WHERE k = ?");
			s.setString(1, finalKey);
			s.executeUpdate();
			
			s.close();
	
		} catch (SQLException e) {
//			plugin.logDbError( e );
		}
		try {
			conn.close();
		} catch (SQLException e) {
//			plugin.logDbError( e );
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
		Connection conn = Prism.dbc();
        PreparedStatement s;
		try {
			
			String finalKey = key;
			if( player != null ){
				finalKey = getPlayerKey(player,key);
			}
			
			s = conn.prepareStatement ("DELETE FROM prism_meta WHERE k = ?");
			s.setString(1, finalKey);
			s.executeUpdate();
			
			s = conn.prepareStatement ("INSERT INTO prism_meta (k,v) VALUES (?,?)");
			s.setString(1, finalKey);
			s.setString(2, value);
			s.executeUpdate();
			
			s.close();
	
		} catch (SQLException e) {
//			plugin.logDbError( e );
		}
		try {
			conn.close();
		} catch (SQLException e) {
//			plugin.logDbError( e );
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
		try {
			
			String finalKey = key;
			if( player != null ){
				finalKey = getPlayerKey(player,key);
			}

			Connection conn = Prism.dbc();
            PreparedStatement s;
    		s = conn.prepareStatement ("SELECT v FROM prism_meta WHERE k = ? LIMIT 0,1");
    		s.setString(1, finalKey);
    		ResultSet rs = s.executeQuery();

    		while(rs.next()){
    			value = rs.getString("v");
			}
    		
    		rs.close();
    		s.close();
    		conn.close();
            
        } catch (SQLException e) {
//        	plugin.logDbError( e );
        }
		return value;
	}
}