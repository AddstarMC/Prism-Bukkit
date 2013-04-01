package me.botsko.prism;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

import me.botsko.prism.settings.Settings;


public class Updater {
	
	/**
	 * 
	 */
	protected final int currentDbSchemaVersion = 4;
	
	/**
	 * 
	 */
	protected Prism plugin;

	
	/**
	 * 
	 * @param plugin
	 */
	public Updater( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * Get the current database schema version
	 * @param person
	 * @param account_name
	 */
	protected int getClientDbSchemaVersion(){
		String schema_ver = Settings.getSetting("schema_ver");
		if( schema_ver != null ){
			return Integer.parseInt(schema_ver);
		}
		return currentDbSchemaVersion;
	}
	
	
	/**
	 * run any queries lower than current currentDbSchemaVersion
	 */
	public void apply_updates(){
		
		int clientSchemaVer = getClientDbSchemaVersion();
		
		// Apply any updates for schema 1 -> 2
		if(clientSchemaVer < 2){
			if( plugin.getConfig().getString("prism.database.mode").equalsIgnoreCase("mysql") ){
		        PreparedStatement s = null;
		        Connection conn = null;
				try {
					
					conn = Prism.dbc();
					
					Prism.log("Applying database updates to schema v2. This may take a while.");
					
					s = conn.prepareStatement("ALTER TABLE `prism_actions` ADD INDEX ( `action_type` ) ;");
					s.executeUpdate();
					
					s = conn.prepareStatement ("ALTER TABLE `prism_actions` ADD INDEX ( `player` ) ;");
					s.executeUpdate();
		
				} catch (SQLException e) {
					plugin.handleDatabaseException( e );
				} finally {
		        	if(s != null) try { s.close(); } catch (SQLException e) {}
		        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
		        }
			}
		}
		
		
		// Apply any updates for schema 2 -> 3
		if(clientSchemaVer < 3){
			if( plugin.getConfig().getString("prism.database.mode").equalsIgnoreCase("mysql") ){
		        PreparedStatement s = null;
		        Connection conn = null;
				try {
					
					conn = Prism.dbc();
					
					Prism.log("Applying database updates to schema v3. This may take a while.");
					
					s = conn.prepareStatement("ALTER TABLE `prism_actions` CHANGE `action_time` `action_time` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;");
					s.executeUpdate();
					
				} catch (SQLException e) {
					plugin.handleDatabaseException( e );
				} finally {
		        	if(s != null) try { s.close(); } catch (SQLException e) {}
		        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
		        }
			} else {
				
				// Sqlite doesn't have any support for altering columns. WTF
				Connection conn = null;
		        PreparedStatement s = null;
		        Statement st = null;
				try {
					
					conn = Prism.dbc();
					
					Prism.log("Applying database updates to schema v3. This may take a while.");
					
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
					st = conn.createStatement();
					st.executeUpdate(query);
					
					s = conn.prepareStatement("INSERT INTO prism_actions (action_type,player,world,x,y,z,data) SELECT action_type,player,world,x,y,z,data FROM tmp_prism_actions;");
					s.executeUpdate();
					
					s = conn.prepareStatement("DROP TABLE tmp_prism_actions;");
					s.executeUpdate();

				} catch (SQLException e) {
					e.printStackTrace();
				} finally {
		        	if(s != null) try { s.close(); } catch (SQLException e) {}
		        	if(st != null) try { st.close(); } catch (SQLException e) {}
		        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
		        }
			}
		}
		
		
		// Apply any updates for schema 3 -> 4
		if(clientSchemaVer < 4){
			if( plugin.getConfig().getString("prism.database.mode").equalsIgnoreCase("mysql") ){
		        PreparedStatement s = null;
		        Connection conn = null;
				try {
					
					conn = Prism.dbc();
					
					Prism.log("Applying database updates to schema v4. This may take a while.");
					
					s = conn.prepareStatement("ALTER TABLE `prism_actions` ADD `block_id` MEDIUMINT( 5 ) NULL AFTER `z`, ADD `block_subid` MEDIUMINT( 5 ) NULL AFTER `block_id`, ADD INDEX ( `block_id` ), CHANGE `data` `data` VARCHAR( 255 ) CHARACTER SET latin1 COLLATE latin1_swedish_ci NULL, ADD `old_block_id` MEDIUMINT( 5 ) NULL AFTER `block_subid` , ADD `old_block_subid` MEDIUMINT( 5 ) NULL AFTER `old_block_id`;");
					s.executeUpdate();
	
				} catch (SQLException e) {
					plugin.handleDatabaseException( e );
				} finally {
		        	if(s != null) try { s.close(); } catch (SQLException e) {}
		        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
		        }
			} else {
				
				// Sqlite doesn't have any support for altering columns. WTF
				Connection conn = null;
		        PreparedStatement s = null;
		        Statement st = null;
				try {
					
					conn = Prism.dbc();
					
					Prism.log("Applying database updates to schema v4. This may take a while.");
					
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
			        		"block_id INT," +
			        		"block_subid INT," +
			        		"old_block_id INT," +
			        		"old_block_subid INT," +
			        		"data TEXT" +
			        		")";
					st = conn.createStatement();
					st.executeUpdate(query);
					
					s = conn.prepareStatement("INSERT INTO prism_actions (action_type,player,world,x,y,z,data) SELECT action_type,player,world,x,y,z,data FROM tmp_prism_actions;");
					s.executeUpdate();
					
					s = conn.prepareStatement("DROP TABLE tmp_prism_actions;");
					s.executeUpdate();

				} catch (SQLException e) {
					e.printStackTrace();
				} finally {
		        	if(s != null) try { s.close(); } catch (SQLException e) {}
		        	if(st != null) try { st.close(); } catch (SQLException e) {}
		        	if(conn != null) try { conn.close(); } catch (SQLException e) {}
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
		Settings.saveSetting("schema_ver", ""+currentDbSchemaVersion);
	}
}
