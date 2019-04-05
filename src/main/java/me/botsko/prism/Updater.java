package me.botsko.prism;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import me.botsko.prism.database.PrismDataSourceUpdater;
import me.botsko.prism.database.PrismDatabaseFactory;
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
		PrismDataSourceUpdater prismDataSourceUpdater = PrismDatabaseFactory.createUpdater(Prism.config);

		updates.add(prismDataSourceUpdater::v1_to_v2);
		updates.add(prismDataSourceUpdater::v2_to_v3);
		updates.add(prismDataSourceUpdater::v3_to_v4);
		updates.add(prismDataSourceUpdater::v4_to_v5);
		updates.add(prismDataSourceUpdater::v5_to_v6);
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

		for (int i = clientSchemaVer; i < currentDbSchemaVersion; ++i) {
			Runnable update = updates.get(i - 1);

			if (update != null) {
				Prism.log("Updating prism schema v" + i + " to v" + (i + 1) + ". This make take a while.");
				update.run();
			}
		}

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