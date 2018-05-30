package me.botsko.prism;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import me.botsko.prism.settings.Settings;

public class Updater {

	/**
	 * 
	 */
	protected final int currentDbSchemaVersion = 6;
	
	private final ArrayList<Runnable> updates = new ArrayList<>(currentDbSchemaVersion);

	/**
	 * 
	 */
	protected final Prism plugin;

	/**
	 * 
	 * @param plugin
	 */
	public Updater(Prism plugin) {
		this.plugin = plugin;

		updates.add(this::v1_to_v2);
		updates.add(this::v2_to_v3);
		updates.add(this::v3_to_v4);
		updates.add(this::v4_to_v5);
		updates.add(this::v5_to_v6);
	}
	
	private void v1_to_v2() {
	}
	
	private void v2_to_v3() {
	}
	
	private void v3_to_v4() {
	}
	
	private void v4_to_v5() {
	}
	
	private void v5_to_v6() {
		String prefix = Prism.config.getString("prism.mysql.prefix");
		Connection conn = Prism.dbc();
		Statement st = null;
		String query;
		
		try {
			st = conn.createStatement();
			
			// Key must be dropped before we can edit colum types
			query = "ALTER TABLE `" + prefix + "data_extra` DROP FOREIGN KEY `" + prefix
					+ "data_extra_ibfk_1`;";
			st.executeUpdate(query);
			
			query = "ALTER TABLE " + prefix + "data MODIFY id bigint(20) unsigned NOT NULL AUTO_INCREMENT";
			st.executeUpdate(query);
			
			query = "ALTER TABLE " + prefix + "data_extra MODIFY extra_id bigint(20) unsigned NOT NULL AUTO_INCREMENT, MODIFY data_id bigint(20) unsigned NOT NULL";
			st.executeUpdate(query);
			
			// return foreign key
			/// BEGIN COPY PASTE Prism.setupDatabase()
			query = "ALTER TABLE `" + prefix + "data_extra` ADD CONSTRAINT `" + prefix
					+ "data_extra_ibfk_1` FOREIGN KEY (`data_id`) REFERENCES `" + prefix
					+ "data` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION;";
			st.executeUpdate(query);
			/// END COPY PASTE
		}
		catch(SQLException e) {
			plugin.handleDatabaseException( e );
		}
		finally {
			if(st != null) try { st.close(); } catch (SQLException e) {}
			if(conn != null) try { conn.close(); } catch (SQLException e) {}
		}
	}

	/**
	 * Get the current database schema version
	 */
	protected int getClientDbSchemaVersion() {
		final String schema_ver = Settings.getSetting("schema_ver");
		if (schema_ver != null) {
			return Integer.parseInt(schema_ver);
		}
		return currentDbSchemaVersion;
	}

	/**
	 * run any queries lower than current currentDbSchemaVersion
	 */
	public void apply_updates() {

		int clientSchemaVer = getClientDbSchemaVersion();
		
		for(int i = clientSchemaVer; i < currentDbSchemaVersion; ++i) {
			Runnable update = updates.get(i - 1);
			
			if(update != null) {
				Prism.log("Updating prism schema v" + i +" to v" + (i + 1) + ". This make take a while.");
				update.run();
			}
		}
		

		// // Apply any updates for schema 1 -> 2
		// if(clientSchemaVer < 2){
		// PreparedStatement s = null;
		// Connection conn = null;
		// try {
		//
		// conn = Prism.dbc();
		//
		// Prism.log("Applying database updates to schema. This may take a while.");
		//
		// s = conn.prepareStatement("");
		// s.executeUpdate();
		//
		// } catch (SQLException e) {
		// plugin.handleDatabaseException( e );
		// } finally {
		// if(s != null) try { s.close(); } catch (SQLException e) {}
		// if(conn != null) try { conn.close(); } catch (SQLException e) {}
		// }
		// }

		// Save current version
		saveCurrentSchemaVersion();
		Prism.log("Update complete: Schema v" + currentDbSchemaVersion);
	}

	/**
	 * Saves the current schema version to the client's db.
	 */
	public void saveCurrentSchemaVersion() {
		Settings.saveSetting("schema_ver", "" + currentDbSchemaVersion);
	}
}