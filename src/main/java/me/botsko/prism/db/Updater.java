package me.botsko.prism.db;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import me.botsko.prism.Prism;

public class Updater {
	
	/**
	 * 
	 */
	protected final int currentDbSchemaVersion = 3;
	
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
		conn = Prism.dbc();
	}
	
	
	/**
	 * Get the current database schema version
	 * @param person
	 * @param account_name
	 */
	protected int getClientDbSchemaVersion(){
		int id = 0;
		try {

            PreparedStatement s;
    		s = conn.prepareStatement ("SELECT v FROM prism_meta WHERE k = 'schema_ver' LIMIT 0,1");
    		ResultSet rs = s.executeQuery();

    		while(rs.next()){
    			id = rs.getInt("v");
			}
    		
    		rs.close();
    		s.close();
            
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
		        PreparedStatement s;
				try {
					
					plugin.log("Applying database updates to schema v2. This may take a while.");
					
					s = conn.prepareStatement("ALTER TABLE `prism_actions` ADD INDEX ( `action_type` ) ;");
					s.executeUpdate();
					
					s = conn.prepareStatement ("ALTER TABLE `prism_actions` ADD INDEX ( `player` ) ;");
					s.executeUpdate();
					s.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
		}
		
		
		// Apply any updates for schema 1 -> 2
		if(clientSchemaVer < 3){
			if( plugin.getConfig().getString("prism.database.mode").equalsIgnoreCase("mysql") ){
		        PreparedStatement s;
				try {
					
					plugin.log("Applying database updates to schema v3. This may take a while.");
					
					s = conn.prepareStatement("ALTER TABLE `prism_actions` CHANGE `action_time` `action_time` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;");
					s.executeUpdate();
					
					s.close();

				} catch (SQLException e) {
					e.printStackTrace();
				}
			} else {
				
				// Sqlite doesn't have any support for altering columns. WTF
		        PreparedStatement s;
				try {
					
					plugin.log("Applying database updates to schema v3. This may take a while.");
					
					s = conn.prepareStatement("ALTER TABLE prism_actions RENAME TO tmp_prism_actions;");
					s.executeUpdate();
					
					String query = "CREATE TABLE IF NOT EXISTS `prism_actions` (" +
			        		"id INT PRIMARY KEY," +
			        		"action_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP," +
			        		"action_type TEXT," +
			        		"player TEXT," +
			        		"world TEXT," +
			        		"x INT," +
			        		"y INT," +
			        		"z INT," +
			        		"data TEXT" +
			        		")";
					Statement st = conn.createStatement();
					st.executeUpdate(query);
					
					s = conn.prepareStatement("INSERT INTO prism_actions (action_type,player,world,x,y,z,data) SELECT action_type,player,world,x,y,z,data FROM tmp_prism_actions;");
					s.executeUpdate();
					
					s = conn.prepareStatement("DROP TABLE tmp_prism_actions;");
					s.executeUpdate();
					
					s.close();

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
        PreparedStatement s;
		try {
			
			s = conn.prepareStatement ("DELETE FROM prism_meta WHERE k = 'schema_ver'");
			s.executeUpdate();
			
			s = conn.prepareStatement ("INSERT INTO prism_meta (k,v) VALUES ('schema_ver',?)");
			s.setInt(1, currentDbSchemaVersion);
			s.executeUpdate();
			
			s.close();
	
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}
}
