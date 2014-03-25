package me.botsko.prism.actionlibs;

import java.util.List;

import org.bukkit.GameMode;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;

public class Ignore {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
	 * 
	 */
    private final List<String> ignore_players;

    /**
	 * 
	 */
    private final List<String> ignore_worlds;

    /**
	 * 
	 */
    private final boolean ignore_creative;

    /**
     * 
     * @param plugin
     */
    @SuppressWarnings("unchecked")
    public Ignore(Prism plugin) {
        this.plugin = plugin;
        ignore_players = (List<String>) plugin.getConfig().getList( "prism.ignore.players" );
        ignore_worlds = (List<String>) plugin.getConfig().getList( "prism.ignore.worlds" );
        ignore_creative = plugin.getConfig().getBoolean( "prism.ignore.players-in-creative" );
    }

    /**
     * 
     * @param actionTypeName
     * @return
     */
    public boolean event(String actionTypeName) {

        // Always track Prism actions - it's mainly internal
        // use anyway.
        if( actionTypeName.contains( "prism" ) ) { return true; }

        // Should we ignore this action type?
        if( ( TypeUtils.subStrOccurences( actionTypeName, "-" ) == 1 && !plugin.getConfig().getBoolean(
                "prism.tracking." + actionTypeName ) ) ) {
            // Prism.debug("Ignoring action type " + actionTypeName);
            return false;
        }

        return true;
    }

    /**
     * 
     * @param actionTypeName
     * @param world
     * @param player
     * @return
     */
    public boolean event(String actionTypeName, World world, String player) {

        return event( actionTypeName, world ) && event( player );

    }

    /**
     * 
     * @param actionTypeName
     * @param player
     * @return
     */
    public boolean event(String actionTypeName, Player player) {

        if( !event( actionTypeName, player.getWorld() ) ) { return false; }

        // Does the player have perms to ignore this action type?
        if( plugin.getConfig().getBoolean( "prism.ignore.enable-perm-nodes" )
                && player.hasPermission( "prism.ignore.tracking." + actionTypeName ) ) {
            Prism.debug( "Player has permission node to ignore " + actionTypeName );
            return false;
        }

        return event( player );

    }

    /**
     * 
     * @param player
     * @return
     */
    public boolean event(Player player) {

        if( player == null || player.getName() == null ) {
            Prism.debug( "Player is being ignored, name is null" );
            return false;
        }

        // Should we ignore this player?
        if( ignore_players != null && ignore_players.contains( player.getName() ) ) {
            Prism.debug( "Player is being ignored, per config: " + player.getName() );
            return false;
        }

        // Should we ignore this player for being in creative?
        if( ignore_creative ) {
            if( player.getGameMode().equals( GameMode.CREATIVE ) ) {
                Prism.debug( "Player is in creative mode, creative mode ignored: " + player.getName() );
                return false;
            }
        }

        return true;
    }

    /**
     * 
     * @param actionTypeName
     * @param block
     * @return
     */
    public boolean event(String actionTypeName, Block block) {

        return event( actionTypeName, block.getWorld() );

    }

    /**
     * 
     * @param actionTypeName
     * @param world
     * @return
     */
    public boolean event(String actionTypeName, World world) {

        // Should we ignore this world?
        if( ignore_worlds != null && ignore_worlds.contains( world.getName() ) ) {
            Prism.debug( "World is being ignored, per config: " + world.getName() );
            return false;
        }

        return event( actionTypeName );

    }
}