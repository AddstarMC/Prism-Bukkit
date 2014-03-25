package me.botsko.prism;

import me.botsko.prism.settings.Settings;

public class Updater {

    /**
	 * 
	 */
    protected final int currentDbSchemaVersion = 5;

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
    }

    /**
     * Get the current database schema version
     */
    protected int getClientDbSchemaVersion() {
        final String schema_ver = Settings.getSetting( "schema_ver" );
        if( schema_ver != null ) { return Integer.parseInt( schema_ver ); }
        return currentDbSchemaVersion;
    }

    /**
     * run any queries lower than current currentDbSchemaVersion
     */
    public void apply_updates() {

        // int clientSchemaVer = getClientDbSchemaVersion();

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

    }

    /**
     * Saves the current schema version to the client's db.
     */
    public void saveCurrentSchemaVersion() {
        Settings.saveSetting( "schema_ver", "" + currentDbSchemaVersion );
    }
}