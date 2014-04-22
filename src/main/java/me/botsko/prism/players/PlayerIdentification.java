package me.botsko.prism.players;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.UUID;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

public class PlayerIdentification {


    /**
     * Loads `prism_players` ID for a real player into our cache.
     *
     * Runs during PlayerJoin events, so it will never be for a fake/null
     * player.
     *
     * @param player
     */
    public static PrismPlayer cachePrismPlayer( final Player player ){

        // Lookup the player
        PrismPlayer prismPlayer = getPrismPlayer( player );
        if( prismPlayer != null ){
            prismPlayer = comparePlayerToCache( player, prismPlayer );
            Prism.debug("Loaded player " + player.getName() + ", id: " + prismPlayer.getId() + " into the cache.");
            Prism.prismPlayers.put( player.getUniqueId(), prismPlayer );
            return prismPlayer;
        }

        // Player is new, create a record for them
        prismPlayer = addPlayer( player );

        return prismPlayer;

    }

    /**
     * Loads `prism_players` ID for a real player into our cache.
     *
     * Runs during PlayerJoin events, so it will never be for a fake/null
     * player.
     *
     * @param player
     */
    public static PrismPlayer cachePrismPlayer( final String playerName ){

        // Lookup the player
        PrismPlayer prismPlayer = getPrismPlayer( playerName );
        if( prismPlayer != null ){
//            prismPlayer = comparePlayerToCache( player, prismPlayer );
            Prism.debug("Loaded player " + prismPlayer.getName() + ", id: " + prismPlayer.getId() + " into the cache.");
//            Prism.prismPlayers.put( player.getUniqueId(), prismPlayer );
            return prismPlayer;
        }

        // Player is new, create a record for them
        prismPlayer = addPlayer( playerName );

        return prismPlayer;

    }


    /**
     * Returns a `prism_players` ID for the described player name. If
     * one cannot be found, returns 0.
     *
     * Used by the recorder in determining proper foreign key
     *
     * @param playerName
     * @return
     */
    public static PrismPlayer getPrismPlayer( String playerName ){

        Player player = Bukkit.getPlayer(playerName);

        if( player != null ) return getPrismPlayer( player );

        // Player not online, we need to go to cache
        PrismPlayer prismPlayer = lookupByName( playerName );

        // Player found! Return the id
        if( prismPlayer != null ) return prismPlayer;

        // No player exists! We must create one
        return null;

    }


    /**
     * Returns a `prism_players` ID for the described player object. If
     * one cannot be found, returns 0.
     *
     * Used by the recorder in determining proper foreign key
     *
     * @param playerName
     * @return
     */
    public static PrismPlayer getPrismPlayer( Player player ){

        if( player.getUniqueId() == null ){
            // If they have a name, we can attempt to find them that way
            if( player.getName() != null && !player.getName().trim().isEmpty() ){
                return getPrismPlayer( player.getName() );
            }
            // No name, no UUID, no service.
            return null;
        }

        PrismPlayer prismPlayer = null;

        // Are they in the cache?
        prismPlayer = Prism.prismPlayers.get( player.getUniqueId() );
        if( prismPlayer != null ) return prismPlayer;

        // Lookup by UUID
        prismPlayer = lookupByUUID( player.getUniqueId() );
        if( prismPlayer != null ) return prismPlayer;

        // Still not found, try looking them up by name
        prismPlayer = lookupByName( player.getName() );
        if( prismPlayer != null ) return prismPlayer;

        return null;

    }


    /**
     * Compares the known player to the cached data. If there's a difference
     * we need to handle it.
     *
     * If usernames are different: Update `prism_players` with new name
     * (@todo track historical?)
     *
     * If UUID is different, log an error.
     *
     * @param player
     * @param prismPlayer
     * @return
     */
    protected static PrismPlayer comparePlayerToCache( Player player, PrismPlayer prismPlayer ){

        // Compare for username differences, update database
        if( !player.getName().equals( prismPlayer.getName() ) ){
            prismPlayer.setName( player.getName() );
            updatePlayer(prismPlayer);
        }

        // Compare UUID
        if( !player.getUniqueId().equals( prismPlayer.getUUID() ) ){
            Prism.log("Player UUID for " +player.getName() + " does not match our cache! " +player.getUniqueId()+ " versus cache of " + prismPlayer.getUUID());

            // Update anyway...
            prismPlayer.setUUID( player.getUniqueId() );
            updatePlayer(prismPlayer);

        }

        return prismPlayer;

    }


    /**
     * Converts UUID to a string ready for use against database
     * @param player
     */
    protected static String uuidToDbString( UUID id ){
        return id.toString().replace("-", "");
    }


    /**
     * Converts UUID to a string ready for use against database
     * @param player
     */
    protected static UUID uuidFromDbString( String uuid ){
        // Positions need to be -2
        String completeUuid = uuid.substring(0, 8);
        completeUuid += "-" + uuid.substring(8,12);
        completeUuid += "-" + uuid.substring(12,16);
        completeUuid += "-" + uuid.substring(16,20);
        completeUuid += "-" + uuid.substring(20, uuid.length());
        completeUuid = completeUuid.toLowerCase();
        return UUID.fromString(completeUuid);
    }


    /**
     * Saves a real player's UUID and current Username to the `prism_players`
     * table. At this stage, we're pretty sure the UUID and username do not
     * already exist.
     * @param player
     */
    protected static PrismPlayer addPlayer( Player player ){
        String prefix = Prism.config.getString("prism.mysql.prefix");

        PrismPlayer prismPlayer = new PrismPlayer( 0, player.getUniqueId(), player.getName() );

        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = Prism.dbc();
            s = conn.prepareStatement( "INSERT INTO " + prefix + "players (player,player_uuid) VALUES (?,UNHEX(?))" , Statement.RETURN_GENERATED_KEYS);
            s.setString(1, player.getName() );
            s.setString(2, uuidToDbString( player.getUniqueId() ) );
            s.executeUpdate();

            rs = s.getGeneratedKeys();
            if (rs.next()) {
                prismPlayer.setId(rs.getInt(1));
                Prism.debug("Saved and loaded player " + player.getName() + " (" + player.getUniqueId() + ") into the cache.");
                Prism.prismPlayers.put( player.getUniqueId(), new PrismPlayer( rs.getInt(1), player.getUniqueId(), player.getName() ) );
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if(rs != null) try { rs.close(); } catch (SQLException e) {}
            if(s != null) try { s.close(); } catch (SQLException e) {}
            if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
        return prismPlayer;
    }


    /**
     * Saves a fake player's name and generated UUID to the `prism_players`
     * table. At this stage, we're pretty sure the UUID and username do not
     * already exist.
     *
     * @param playerName
     * @return
     */
    protected static PrismPlayer addPlayer( String playerName ){
        String prefix = Prism.config.getString("prism.mysql.prefix");

        PrismPlayer fakePlayer = new PrismPlayer( 0, UUID.randomUUID(), playerName );

        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = Prism.dbc();
            s = conn.prepareStatement( "INSERT INTO " + prefix + "players (player,player_uuid) VALUES (?,UNHEX(?))" , Statement.RETURN_GENERATED_KEYS);
            s.setString(1, fakePlayer.getName() );
            s.setString(2, uuidToDbString( fakePlayer.getUUID() ) );
            s.executeUpdate();

            rs = s.getGeneratedKeys();
            if (rs.next()){
                fakePlayer.setId( rs.getInt(1) );
                Prism.debug("Saved and loaded fake player " + fakePlayer.getName() + " into the cache.");
                Prism.prismPlayers.put( fakePlayer.getUUID(), fakePlayer );
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if(rs != null) try { rs.close(); } catch (SQLException e) {}
            if(s != null) try { s.close(); } catch (SQLException e) {}
            if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
        return fakePlayer;
    }


    /**
     * Saves a player's UUID to the prism_players table. We cache the current username
     * as well.
     */
    protected static void updatePlayer( PrismPlayer prismPlayer ){
        String prefix = Prism.config.getString("prism.mysql.prefix");

        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = Prism.dbc();
            s = conn.prepareStatement( "UPDATE " + prefix + "players SET player = ?, player_uuid = UNHEX(?) WHERE player_id = ?");
            s.setString(1, prismPlayer.getName() );
            s.setString(2, uuidToDbString( prismPlayer.getUUID() ) );
            s.setInt(3, prismPlayer.getId() );
            s.executeUpdate();

        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if(rs != null) try { rs.close(); } catch (SQLException e) {}
            if(s != null) try { s.close(); } catch (SQLException e) {}
            if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
    }


    /**
     * Loads `prism_players` ID for a player into our cache.
     */
    protected static PrismPlayer lookupByName( String playerName ){
        String prefix = Prism.config.getString("prism.mysql.prefix");
        PrismPlayer prismPlayer = null;
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = Prism.dbc();
            s = conn.prepareStatement( "SELECT player_id, player, HEX(player_uuid) FROM " + prefix + "players WHERE player = ?" );
            s.setString(1, playerName);
            rs = s.executeQuery();

            if( rs.next() ){
                prismPlayer = new PrismPlayer( rs.getInt(1), uuidFromDbString(rs.getString(3)), rs.getString(2) );
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if(rs != null) try { rs.close(); } catch (SQLException e) {}
            if(s != null) try { s.close(); } catch (SQLException e) {}
            if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
        return prismPlayer;
    }


    /**
     * Loads `prism_players` ID for a player into our cache.
     */
    protected static PrismPlayer lookupByUUID( UUID uuid ){
        String prefix = Prism.config.getString("prism.mysql.prefix");
        PrismPlayer prismPlayer = null;
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = Prism.dbc();
            s = conn.prepareStatement( "SELECT player_id, player, HEX(player_uuid) FROM " + prefix + "players WHERE player_uuid = UNHEX(?)" );
            s.setString(1, uuidToDbString(uuid));
            rs = s.executeQuery();

            if( rs.next() ){
                prismPlayer = new PrismPlayer( rs.getInt(1), uuidFromDbString(rs.getString(3)), rs.getString(2) );
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if(rs != null) try { rs.close(); } catch (SQLException e) {}
            if(s != null) try { s.close(); } catch (SQLException e) {}
            if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
        return prismPlayer;
    }


    /**
     * Build-load all online players into cache
     */
    public static void cacheOnlinePlayerPrimaryKeys(){
        String prefix = Prism.config.getString("prism.mysql.prefix");

        String[] playerNames;
        playerNames = new String[ Bukkit.getServer().getOnlinePlayers().length ];
        int i = 0;
        for( Player pl : Bukkit.getServer().getOnlinePlayers() ){
            playerNames[i] = pl.getName();
            i++;
        }

        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = Prism.dbc();
            s = conn.prepareStatement( "SELECT player_id, player, HEX(player_uuid) FROM " + prefix + "players WHERE player IN (?)" );
            s.setString(1, "'"+TypeUtils.join(playerNames, "','")+"'");
            rs = s.executeQuery();

            while( rs.next() ){
                PrismPlayer prismPlayer = new PrismPlayer( rs.getInt(1), uuidFromDbString(rs.getString(3)), rs.getString(2) );
                Prism.debug("Loaded player " + rs.getString(2) + ", id: " + rs.getInt(1) + " into the cache.");
                Prism.prismPlayers.put( UUID.fromString(rs.getString(2)), prismPlayer );
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if(rs != null) try { rs.close(); } catch (SQLException e) {}
            if(s != null) try { s.close(); } catch (SQLException e) {}
            if(conn != null) try { conn.close(); } catch (SQLException e) {}
        }
    }
}