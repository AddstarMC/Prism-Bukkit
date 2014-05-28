package me.botsko.prism.commandlibs;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.parameters.PrismParameterHandler;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import java.util.*;
import java.util.Map.Entry;

public class PreprocessArgs {

    /**
     *
     */
    public static QueryParameters process(Prism plugin, CommandSender sender, String[] args,
                                          PrismProcessType processType, int startAt, boolean useDefaults) {
        return process(plugin, sender, args, processType, startAt, useDefaults, false);
    }

    /**
     *
     */
    public static QueryParameters process(Prism plugin, CommandSender sender, String[] args,
            PrismProcessType processType, int startAt, boolean useDefaults, boolean optional) {

        // Check for player or sender
        Player player = null;
        if( sender != null && sender instanceof Player ) {
            player = (Player) sender;
        }

        // Start query
        final QueryParameters parameters = new QueryParameters();
        parameters.setProcessType( processType );

        // Define pagination/process type
        if( parameters.getProcessType().equals( PrismProcessType.LOOKUP ) ) {
            parameters.setLimit( plugin.getConfig().getInt( "prism.queries.lookup-max-results" ) );
            parameters.setPerPage( plugin.getConfig().getInt( "prism.queries.default-results-per-page" ) );
        }

        // Load registered parameters
        final HashMap<String, PrismParameterHandler> registeredParams = Prism.getParameters();

        // Store names of matched params/handlers
        final Set<String> foundArgsNames = new HashSet<String>();
        final List<MatchedParam> foundArgsList = new ArrayList<MatchedParam>();

        // Iterate all command arguments
        if (args == null) {
            return parameters;
        }

        for ( int i = startAt; i < args.length; i++ ) {

            final String arg = args[i];
            if( arg.isEmpty() )
                continue;

            if (parseParam(plugin, sender, parameters, registeredParams, foundArgsNames, foundArgsList, arg) == ParseResult.NotFound)
                return null;
        }
        parameters.setFoundArgs( foundArgsNames );

        // Reject no matches
        if( foundArgsList.isEmpty() && !optional ) {
            if( sender != null )
                sender.sendMessage( Prism.messenger
                        .playerError( "You're missing valid parameters. Use /prism ? for assistance." ) );
            return null;
        }

        /**
         * Call default method for handlers *not* used
         */
        if( useDefaults ) {
            for ( final Entry<String, PrismParameterHandler> entry : registeredParams.entrySet() ) {
                if( !foundArgsNames.contains( entry.getKey().toLowerCase() ) ) {
                    entry.getValue().defaultTo( parameters, sender );
                }
            }
        }

        /**
         * Send arguments to parameter handlers
         */
        for ( final MatchedParam matchedParam : foundArgsList ) {
            try {
                final PrismParameterHandler handler = matchedParam.getHandler();
                handler.process( parameters, matchedParam.getArg(), sender );
            } catch ( final IllegalArgumentException e ) {
                if( sender != null )
                    sender.sendMessage( Prism.messenger.playerError( e.getMessage() ) );
                return null;
            }
        }

        // Player location
        if( player != null && !plugin.getConfig().getBoolean( "prism.queries.never-use-defaults" )
                && parameters.getPlayerLocation() == null
                && ( parameters.getMaxLocation() == null || parameters.getMinLocation() == null ) ) {
            parameters.setMinMaxVectorsFromPlayerLocation( player.getLocation() );
        }
        return parameters;
    }

    /**
     * 
     * @param plugin
     * @param sender
     * @param parameters
     * @param registeredParams
     * @param foundArgsNames
     * @param foundArgsList
     * @param arg
     * @return
     */
    private static ParseResult parseParam(Prism plugin, CommandSender sender, QueryParameters parameters, HashMap<String, PrismParameterHandler> registeredParams, Set<String> foundArgsNames, List<MatchedParam> foundArgsList, String arg) {
        ParseResult result = ParseResult.NotFound;

        // Match command argument to parameter handler
        for ( final Entry<String, PrismParameterHandler> entry : registeredParams.entrySet() ) {
            PrismParameterHandler parameterHandler = entry.getValue();
            if (!parameterHandler.applicable(arg, sender)) {
                continue;
            }
            if( !parameterHandler.hasPermission(arg, sender) ) {
                result = ParseResult.NoPermission;
                continue;
            }
            result = ParseResult.Found;
            foundArgsList.add( new MatchedParam(parameterHandler, arg ) );
            foundArgsNames.add( parameterHandler.getName().toLowerCase() );
            break;
        }

        // Reject argument that doesn't match anything
        if( result == ParseResult.NotFound ) {
            // We support an alternate player syntax so that people
            // can use the tab-complete
            // feature of minecraft. Using p: prevents it.
            final Player autoFillPlayer = plugin.getServer().getPlayer( arg );
            if( autoFillPlayer != null ) {
                MatchRule match = MatchRule.INCLUDE;
                if( arg.startsWith( "!" ) ) {
                    match = MatchRule.EXCLUDE;
                }
                result = ParseResult.Found;
                parameters.addPlayerName( arg.replace( "!", "" ), match );
            }
        }

        switch (result) {
            case NotFound:
                if (sender != null)
                    sender.sendMessage(Prism.messenger.playerError("Unrecognized parameter '" + arg
                            + "'. Use /prism ? for help."));
                break;
            case NoPermission:
                if (sender != null)
                    sender.sendMessage(Prism.messenger.playerError("No permission for parameter '" + arg + "', skipped."));
                break;
            default:
                break;
        }
        return result;
    }

    /**
     * 
     * @param sender
     * @param args
     * @param arg
     * @return
     */
    public static List<String> complete(CommandSender sender, String[] args, int arg) {
        // Iterate all command arguments
        if( args == null || args.length <= arg ) { return null; }

        return complete( sender, args[arg] );
    }

    /**
     * 
     * @param sender
     * @param args
     * @return
     */
    public static List<String> complete(CommandSender sender, String[] args) {
        return complete( sender, args, args.length - 1 );
    }

    /**
     * 
     * @param sender
     * @param arg
     * @return
     */
    public static List<String> complete(CommandSender sender, String arg) {
        if( arg.isEmpty() )
            return null;

        // Load registered parameters
        final HashMap<String, PrismParameterHandler> registeredParams = Prism.getParameters();

        // Match command argument to parameter handler
        for ( final Entry<String, PrismParameterHandler> entry : registeredParams.entrySet() ) {
            if( entry.getValue().applicable( arg, sender ) && entry.getValue().hasPermission( arg, sender ) ) { return entry.getValue().tabComplete( arg, sender ); }
        }

        return null;
    }

    /**
	 * 
	 */
    private static class MatchedParam {
        private final PrismParameterHandler handler;
        private final String arg;

        public MatchedParam(PrismParameterHandler handler, String arg) {
            this.handler = handler;
            this.arg = arg;
        }

        public PrismParameterHandler getHandler() {
            return handler;
        }

        public String getArg() {
            return arg;
        }
    }

    /**
     * 
     * @author botskonet
     *
     */
    private enum ParseResult {
        NotFound,
        NoPermission,
        Found
    }
}