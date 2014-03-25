package me.botsko.prism.parameters;

import me.botsko.elixr.ChunkUtils;
import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.bridge.WorldEditBridge;
import me.botsko.prism.utils.MiscUtils;
import org.bukkit.Bukkit;
import org.bukkit.Chunk;
import org.bukkit.Location;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;

import java.util.regex.Pattern;

public class RadiusParameter extends SimplePrismParameterHandler {

    /**
	 * 
	 */
    public RadiusParameter() {
        super( "Radius", Pattern.compile( "[\\w,:-]+" ), "r" );
    }

    /**
	 * 
	 */
    @Override
    public void process(QueryParameters query, String alias, String input, CommandSender sender) {

        Player player = null;
        if( sender instanceof Player ) {
            player = (Player) sender;
        }

        String inputValue = input;

        final FileConfiguration config = Bukkit.getPluginManager().getPlugin( "Prism" ).getConfig();

        if( TypeUtils.isNumeric( inputValue )
                || ( inputValue.contains( ":" ) && inputValue.split( ":" ).length >= 1 && TypeUtils
                        .isNumeric( inputValue.split( ":" )[1] ) ) ) {
            int radius, desiredRadius;
            Location coordsLoc = null;
            if( inputValue.contains( ":" ) ) {
                desiredRadius = Integer.parseInt( inputValue.split( ":" )[1] );
                final String radiusLocOrPlayer = inputValue.split( ":" )[0];
                if( radiusLocOrPlayer.contains( "," ) && player != null ) { // Coordinates;
                                                                            // x,y,z
                    final String[] coordinates = radiusLocOrPlayer.split( "," );
                    if( coordinates.length != 3 ) { throw new IllegalArgumentException(
                            "Couldn't parse the coordinates '" + radiusLocOrPlayer
                                    + "'. Perhaps you have more than two commas?" ); }
                    for ( final String s : coordinates ) {
                        if( !TypeUtils.isNumeric( s ) ) { throw new IllegalArgumentException( "The coordinate '" + s
                                + "' is not a number." ); }
                    }
                    coordsLoc = ( new Location( player.getWorld(), Integer.parseInt( coordinates[0] ),
                            Integer.parseInt( coordinates[1] ), Integer.parseInt( coordinates[2] ) ) );

                }
                // Try to find an online player
                else if( Bukkit.getServer().getPlayer( radiusLocOrPlayer ) != null ) {
                    player = Bukkit.getServer().getPlayer( radiusLocOrPlayer );
                } else {
                    throw new IllegalArgumentException( "Couldn't find the player named '" + radiusLocOrPlayer
                            + "'. Perhaps they are not online or you misspelled their name?" );
                }
            } else {
                desiredRadius = Integer.parseInt( inputValue );
            }
            if( desiredRadius <= 0 ) { throw new IllegalArgumentException(
                    "Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help." ); }

            // If neither sender or a named player found, die here
            if( player == null ) { throw new IllegalArgumentException(
                    "The radius parameter must be used by a player. Use w:worldname if attempting to limit to a world." ); }

            // Clamp radius based on perms, configs
            radius = MiscUtils.clampRadius( player, desiredRadius, query.getProcessType(), config );
            if( desiredRadius != radius ) {
                if( sender != null )
                    sender.sendMessage( Prism.messenger.playerError( "Forcing radius to " + radius
                            + " as allowed by config." ) );
            }

            if( radius > 0 ) {
                query.setRadius( radius );
                if( coordsLoc != null ) {
                    query.setMinMaxVectorsFromPlayerLocation( coordsLoc ); // We
                                                                           // need
                                                                           // to
                                                                           // set
                                                                           // this
                                                                           // *after*
                                                                           // the
                                                                           // radius
                                                                           // has
                                                                           // been
                                                                           // set
                                                                           // or
                                                                           // it
                                                                           // won't
                                                                           // work.
                } else {
                    query.setMinMaxVectorsFromPlayerLocation( player.getLocation() );
                }
            }
        } else {

            // If neither sender or a named player found, die here
            if( player == null ) { throw new IllegalArgumentException(
                    "The radius parameter must be used by a player. Use w:worldname if attempting to limit to a world." ); }

            // User wants an area inside of a worldedit selection
            if( inputValue.equals( "we" ) ) {

                if( Prism.plugin_worldEdit == null ) {
                    throw new IllegalArgumentException(
                            "This feature is disabled because Prism couldn't find WorldEdit." );
                } else {

                    // Load a selection from world edit as our area.
                    final Prism prism = (Prism) Bukkit.getPluginManager().getPlugin( "Prism" );
                    if( !WorldEditBridge.getSelectedArea( prism, player, query ) ) { throw new IllegalArgumentException(
                            "Invalid region selected. Make sure you have a region selected, and that it doesn't exceed the max radius." ); }
                }
            }

            // Confine to the chunk
            else if( inputValue.equals( "c" ) || inputValue.equals( "chunk" ) ) {

                final Chunk ch = player.getLocation().getChunk();
                query.setWorld( ch.getWorld().getName() );
                query.setMinLocation( ChunkUtils.getChunkMinVector( ch ) );
                query.setMaxLocation( ChunkUtils.getChunkMaxVector( ch ) );

            }

            // User wants no radius, but contained within the current world
            else if( inputValue.equals( "world" ) ) {
                // Do they have permission to override the global lookup radius
                if( query.getProcessType().equals( PrismProcessType.LOOKUP )
                        && !player.hasPermission( "prism.override-max-lookup-radius" ) ) { throw new IllegalArgumentException(
                        "You do not have permission to override the max radius." ); }
                // Do they have permission to override the global applier radius
                if( !query.getProcessType().equals( PrismProcessType.LOOKUP )
                        && !player.hasPermission( "prism.override-max-applier-radius" ) ) { throw new IllegalArgumentException(
                        "You do not have permission to override the max radius." ); }
                // Use the world defined in the w: param
                if( query.getWorld() != null ) {
                    inputValue = query.getWorld();
                }
                // Use the current world
                else {
                    inputValue = player.getWorld().getName();
                }
                query.setWorld( inputValue );
                query.setAllowNoRadius( true );
            }

            // User has asked for a global radius
            else if( inputValue.equals( "global" ) ) {
                // Do they have permission to override the global lookup radius
                if( query.getProcessType().equals( PrismProcessType.LOOKUP )
                        && !player.hasPermission( "prism.override-max-lookup-radius" ) ) { throw new IllegalArgumentException(
                        "You do not have permission to override the max radius." ); }
                // Do they have permission to override the global applier radius
                if( !query.getProcessType().equals( PrismProcessType.LOOKUP )
                        && !player.hasPermission( "prism.override-max-applier-radius" ) ) { throw new IllegalArgumentException(
                        "You do not have permission to override the max radius." ); }
                // Either they have permission or player is null
                query.setWorld( null );
                query.setAllowNoRadius( true );

            } else {
                throw new IllegalArgumentException(
                        "Radius is invalid. There's a bunch of choice, so use /prism actions for assistance." );
            }
        }
    }

    /**
	 * 
	 */
    @Override
    public void defaultTo(QueryParameters query, CommandSender sender) {
        if( query.getProcessType().equals( PrismProcessType.DELETE ) )
            return;
        if( sender != null && sender instanceof Player ) {
            if( query.allowsNoRadius() ) {
                // We'll allow no radius.
            } else {
                final FileConfiguration config = Bukkit.getPluginManager().getPlugin( "Prism" ).getConfig();
                query.setRadius( config.getInt( "prism.queries.default-radius" ) );
                query.addDefaultUsed( "r:" + query.getRadius() );
            }
        }
    }
}