package me.botsko.prism.utils;

import com.google.common.base.CaseFormat;
import com.helion3.pste.api.Paste;
import com.helion3.pste.api.PsteApi;
import com.helion3.pste.api.Results;
import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.appliers.PrismProcessType;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;

import java.util.ArrayList;
import java.util.List;

public class MiscUtils {

    /**
     * Placing this here so it's easier to share the logic
     * 
     * @param player
     * @param desiredRadius
     * @param processType
     * @param config
     * @return
     */
    public static int clampRadius(Player player, int desiredRadius, PrismProcessType processType,
            FileConfiguration config) {

        if( desiredRadius <= 0 ) { return config.getInt( "prism.near.default-radius" ); }

        // Safety checks for max lookup radius
        int max_lookup_radius = config.getInt( "prism.queries.max-lookup-radius" );
        if( max_lookup_radius <= 0 ) {
            max_lookup_radius = 5;
            Prism.log( "Max lookup radius may not be lower than one. Using safe inputue of five." );
        }

        // Safety checks for max applier radius
        int max_applier_radius = config.getInt( "prism.queries.max-applier-radius" );
        if( max_applier_radius <= 0 ) {
            max_applier_radius = 5;
            Prism.log( "Max applier radius may not be lower than one. Using safe inputue of five." );
        }

        // Does the radius exceed the configured max?
        if( processType.equals( PrismProcessType.LOOKUP ) && desiredRadius > max_lookup_radius ) {
            // If player does not have permission to override the max
            if( player != null && !player.hasPermission( "prism.override-max-lookup-radius" ) ) { return max_lookup_radius; }
            // Otherwise non-player
            return desiredRadius;
        } else if( !processType.equals( PrismProcessType.LOOKUP ) && desiredRadius > max_applier_radius ) {
            // If player does not have permission to override the max
            if( player != null && !player.hasPermission( "prism.override-max-applier-radius" ) ) { return max_applier_radius; }
            // Otherwise non-player
            return desiredRadius;
        } else {
            // Otherwise, the radius is valid and is not exceeding max
            return desiredRadius;
        }
    }

    /**
     * 
     * @param prism
     * @param results
     * @return
     */
    public static String paste_results(Prism prism, String results) {

        final String prismWebUrl = "https://pste.me/";

        if( !prism.getConfig().getBoolean( "prism.paste.enable" ) ) { return Prism.messenger
                .playerError( "PSTE.me paste bin support is currently disabled by config." ); }

        final String apiUsername = prism.getConfig().getString( "prism.paste.username" );
        final String apiKey = prism.getConfig().getString( "prism.paste.api-key" );

        if( !apiKey.matches( "[0-9a-z]+" ) ) { return Prism.messenger.playerError( "Invalid API key." ); }

        final PsteApi api = new PsteApi( apiUsername, apiKey );

        try {

            final Paste paste = new Paste();
            paste.setPaste( results );

            final Results response = api.createPaste( paste );
            return Prism.messenger.playerSuccess( "Successfully pasted results: " + prismWebUrl + "#/"
                    + response.getResults().getSlug() );

        } catch ( final Exception up ) {
            Prism.debug( up.toString() );
            return Prism.messenger.playerError( "Unable to paste results (" + ChatColor.YELLOW + up.getMessage()
                    + ChatColor.RED + ")." );
        }
    }

    public static List<String> getStartingWith(String start, Iterable<String> options, boolean caseSensitive) {
        final List<String> result = new ArrayList<String>();
        if( caseSensitive ) {
            for ( final String option : options ) {
                if( option.startsWith( start ) )
                    result.add( option );
            }
        } else {
            start = start.toLowerCase();
            for ( final String option : options ) {
                if( option.toLowerCase().startsWith( start ) )
                    result.add( option );
            }
        }

        return result;
    }

    public static List<String> getStartingWith(String arg, Iterable<String> options) {
        return getStartingWith( arg, options, true );
    }

    public static void dispatchAlert(String msg, List<String> commands) {
        String colorized = TypeUtils.colorize(msg);
        String stripped = ChatColor.stripColor(colorized);
        for (String command : commands) {
            if(command.equals("examplecommand <alert>"))
                continue;
            String processedCommand = command.replace("<alert>", stripped);
            Bukkit.dispatchCommand(Bukkit.getConsoleSender(), processedCommand);
        }
    }

    public static String getEntityName(Entity entity) {
        if(entity == null)
            return "unknown";
        if(entity.getType() == EntityType.PLAYER)
            return ((Player)entity).getName();
        return CaseFormat.UPPER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, entity.getType().name());
    }
}