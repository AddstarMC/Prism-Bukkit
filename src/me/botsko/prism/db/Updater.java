package me.botsko.prism.db;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import me.botsko.prism.Prism;

public class Updater {
	
	/**
	 * 
	 */
	protected final int currentDbSchemaVersion = 2;
	
	/**
	 * 
	 */
	protected Prism plugin;
	
	/**
	 * 
	 */
	protected Connection conn;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public Updater( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 */
	protected void dbc(){
		try {
			if (conn == null || conn.isClosed() || !conn.isValid(1)) {
				conn = plugin.getDbConnection();
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}
	
	
	/**
	 * Get the current database schema version
	 * @param person
	 * @param account_name
	 */
	protected int getClientDbSchemaVersion(){
		int id = 0;
		try {
            
			dbc();
            PreparedStatement s;
    		s = conn.prepareStatement ("SELECT v FROM prism_meta WHERE k = 'schema_ver' LIMIT 0,1");
    		ResultSet rs = s.executeQuery();

    		while(rs.next()){
    			id = rs.getInt("v");
			}
    		
    		rs.close();
    		s.close();
            conn.close();
            
        } catch (SQLException e) {
            e.printStackTrace();
        }
		return id;
	}
	
	
	/**
	 * run any queries lower than current currentDbSchemaVersion
	 */
	public void apply_updates(){
		
		int clientSchemaVer = getClientDbSchemaVersion();
		
		// Apply any updates for schema 1 -> 2
		if(clientSchemaVer < 2){
			if( plugin.getConfig().getString("prism.database.mode").equalsIgnoreCase("mysql") ){
				dbc();
		        PreparedStatement s;
				try {
					
					plugin.log("Applying database updates. This may take a while.");
					
					s = conn.prepareStatement("ALTER TABLE `prism_actions` ADD INDEX ( `action_type` ) ;");
					s.executeUpdate();
					
					s = conn.prepareStatement ("ALTER TABLE `prism_actions` ADD INDEX ( `player` ) ;");
					s.executeUpdate();
					
					s.close();
					conn.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
		}
		
		
		// Save current version
		saveCurrentSchemaVersion();
		
	}
	
	
	/**
	 * Saves the current schema version to the client's db.
	 */
	public void saveCurrentSchemaVersion(){
		dbc();
        PreparedStatement s;
		try {
			
			s = conn.prepareStatement ("DELETE FROM prism_meta WHERE k = 'schema_ver'");
			s.executeUpdate();
			
			s = conn.prepareStatement ("INSERT INTO prism_meta (k,v) VALUES ('schema_ver',?)");
			s.setInt(1, currentDbSchemaVersion);
			s.executeUpdate();
			
			s.close();
			conn.close();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}
}
