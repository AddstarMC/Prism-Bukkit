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
     * @param args
     */
    public static QueryParameters process(Prism plugin, CommandSender sender, String[] args,
            PrismProcessType processType, int startAt, boolean useDefaults) {

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
        if( args != null ) {
            for ( int i = startAt; i < args.length; i++ ) {

                final String arg = args[i];
                if( arg.isEmpty() )
                    continue;

                boolean handlerFound = false;

                // Match command argument to parameter handler
                for ( final Entry<String, PrismParameterHandler> entry : registeredParams.entrySet() ) {
                    if( entry.getValue().applicable( arg, sender ) ) {
                        handlerFound = true;
                        foundArgsList.add( new MatchedParam( entry.getValue(), arg ) );
                        foundArgsNames.add( entry.getValue().getName().toLowerCase() );
                    } else {
                        // We support an alternate player syntax so that people
                        // can use the tab-complete
                        // feature of minecraft. Using p: prevents it.
                        final Player autoFillPlayer = plugin.getServer().getPlayer( arg );
                        if( autoFillPlayer != null ) {
                            MatchRule match = MatchRule.INCLUDE;
                            if( arg.startsWith( "!" ) ) {
                                match = MatchRule.EXCLUDE;
                            }
                            handlerFound = true;
                            parameters.addPlayerName( arg.replace( "!", "" ), match );
                        }
                    }
                }

                // Reject argument that doesn't match anything
                if( !handlerFound ) {
                    if( sender != null )
                        sender.sendMessage( Prism.messenger.playerError( "Unrecognized parameter '" + arg
                                + "'. Use /prism ? for help." ) );
                    return null;
                }
            }
            parameters.setFoundArgs( foundArgsNames );

            // Reject no matches
            if( foundArgsList.isEmpty() ) {
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
        }
        return parameters;
    }

    public static List<String> complete(CommandSender sender, String[] args, int arg) {
        // Iterate all command arguments
        if( args == null || args.length <= arg ) { return null; }

        return complete( sender, args[arg] );
    }

    public static List<String> complete(CommandSender sender, String[] args) {
        return complete( sender, args, args.length - 1 );
    }

    public static List<String> complete(CommandSender sender, String arg) {
        if( arg.isEmpty() )
            return null;

        // Load registered parameters
        final HashMap<String, PrismParameterHandler> registeredParams = Prism.getParameters();

        // Match command argument to parameter handler
        for ( final Entry<String, PrismParameterHandler> entry : registeredParams.entrySet() ) {
            if( entry.getValue().applicable( arg, sender ) ) { return entry.getValue().tabComplete( arg, sender ); }
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
}