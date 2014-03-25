package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.MiscUtils;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import java.util.ArrayList;
import java.util.List;

public class LookupCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     * @return
     */
    public LookupCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(final CallInfo call) {

        // Process and validate all of the arguments
        final QueryParameters parameters = PreprocessArgs.process( plugin, call.getSender(), call.getArgs(),
                PrismProcessType.LOOKUP, 1, !plugin.getConfig().getBoolean( "prism.queries.never-use-defaults" ) );
        if( parameters == null ) { return; }

        /**
         * Run the lookup itself in an async task so the lookup query isn't done
         * on the main thread
         */
        plugin.getServer().getScheduler().runTaskAsynchronously( plugin, new Runnable() {
            @Override
            public void run() {

                // determine if defaults were used
                final ArrayList<String> defaultsUsed = parameters.getDefaultsUsed();
                String defaultsReminder = "";
                if( !defaultsUsed.isEmpty() ) {
                    defaultsReminder += "Using defaults:";
                    for ( final String d : defaultsUsed ) {
                        defaultsReminder += " " + d;
                    }
                }

                final ActionsQuery aq = new ActionsQuery( plugin );
                final QueryResult results = aq.lookup( parameters, call.getSender() );
                String sharingWithPlayers = "";
                for ( final CommandSender shareWith : parameters.getSharedPlayers() ) {
                    sharingWithPlayers += shareWith.getName() + ", ";
                }
                sharingWithPlayers = sharingWithPlayers.substring( 0, sharingWithPlayers.isEmpty() ? 0
                        : sharingWithPlayers.length() - 2 );

                // Add current sender
                parameters.addSharedPlayer( call.getSender() );

                for ( final CommandSender player : parameters.getSharedPlayers() ) {

                    final boolean isSender = player.getName().equals( call.getSender().getName() );

                    if( !isSender ) {
                        player.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.YELLOW + "" + ChatColor.ITALIC
                                + call.getSender().getName() + ChatColor.GOLD
                                + " shared these Prism lookup logs with you:" ) );
                    } else if( !sharingWithPlayers.isEmpty() ) {
                        player.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.GOLD
                                + "Sharing results with players: " + ChatColor.YELLOW + "" + ChatColor.ITALIC
                                + sharingWithPlayers ) );
                    }

                    if( !results.getActionResults().isEmpty() ) {
                        player.sendMessage( Prism.messenger.playerHeaderMsg( "Showing " + results.getTotalResults()
                                + " results. Page 1 of " + results.getTotal_pages() ) );
                        if( !defaultsReminder.isEmpty() && isSender ) {
                            player.sendMessage( Prism.messenger.playerSubduedHeaderMsg( defaultsReminder ) );
                        }
                        final List<Handler> paginated = results.getPaginatedActionResults();
                        if( paginated != null ) {
                            int result_count = results.getIndexOfFirstResult();
                            for ( final Handler a : paginated ) {
                                final ActionMessage am = new ActionMessage( a );
                                if( parameters.allowsNoRadius() || parameters.hasFlag( Flag.EXTENDED )
                                        || plugin.getConfig().getBoolean( "prism.messenger.always-show-extended" ) ) {
                                    am.showExtended();
                                }
                                am.setResultIndex( result_count );
                                player.sendMessage( Prism.messenger.playerMsg( am.getMessage() ) );
                                result_count++;
                            }
                        } else {
                            player.sendMessage( Prism.messenger
                                    .playerError( "Pagination can't find anything. Do you have the right page number?" ) );
                        }
                        if( parameters.hasFlag( Flag.PASTE ) ) {
                            String paste = "";
                            for ( final Handler a : results.getActionResults() ) {
                                paste += new ActionMessage( a ).getRawMessage() + "\r\n";
                            }
                            player.sendMessage( MiscUtils.paste_results( plugin, paste ) );
                        }
                    } else {
                        if( !defaultsReminder.isEmpty() ) {
                            if( isSender ) {
                                player.sendMessage( Prism.messenger.playerSubduedHeaderMsg( defaultsReminder ) );
                            }
                        }
                        if( isSender ) {
                            player.sendMessage( Prism.messenger.playerError( "Nothing found." + ChatColor.GRAY
                                    + " Either you're missing something, or we are." ) );
                        }
                    }
                }

                // Flush timed data
                plugin.eventTimer.printTimeRecord();

            }
        } );
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return PreprocessArgs.complete( call.getSender(), call.getArgs() );
    }
}