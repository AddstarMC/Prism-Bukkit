package me.botsko.prism.commands;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.ChatColor;
import org.bukkit.entity.Player;

import java.util.List;

public class PageCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     * @return
     */
    public PageCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(CallInfo call) {

        // Is there anything even stored to paginate?
        String keyName = "console";
        if( call.getSender() instanceof Player ) {
            keyName = call.getSender().getName();
        }
        if( !plugin.cachedQueries.containsKey( keyName ) ) {
            call.getSender()
                    .sendMessage(
                            Prism.messenger
                                    .playerError( "There's no saved query to paginate. Maybe they expired? Try your lookup again." ) );
            return;
        }

        // Get stored results
        final QueryResult results = plugin.cachedQueries.get( keyName );

        if( call.getArgs().length != 2 ) {
            call.getSender().sendMessage(
                    Prism.messenger.playerError( "Please specify a page number. Like /prism page 2" ) );
            return;
        }

        // Determine page number
        int page;
        if( TypeUtils.isNumeric( call.getArg( 1 ) ) ) {
            page = Integer.parseInt( call.getArg( 1 ) );
        } else {

            // Next page
            if( call.getArg( 1 ).equals( "next" ) || call.getArg( 1 ).equals( "n" ) ) {
                page = results.getPage() + 1;
            }
            // Previous page
            else if( call.getArg( 1 ).equals( "prev" ) || call.getArg( 1 ).equals( "p" ) ) {
                if( results.getPage() <= 1 ) {
                    call.getSender().sendMessage( Prism.messenger.playerError( "There is no previous page." ) );
                    return;
                }
                page = results.getPage() - 1;
            } else {
                call.getSender()
                        .sendMessage(
                                Prism.messenger
                                        .playerError( "Page numbers need to actually be numbers, or next/prev. Like /prism page 2" ) );
                return;
            }
        }

        // No negatives
        if( page <= 0 ) {
            call.getSender().sendMessage( Prism.messenger.playerError( "Page must be greater than zero." ) );
            return;
        }

        results.setPage( page );

        // Refresh the query time and replace
        results.setQueryTime();
        plugin.cachedQueries.replace( keyName, results );

        // Results?
        if( results.getActionResults().isEmpty() ) {
            call.getSender().sendMessage(
                    Prism.messenger.playerError( "Nothing found." + ChatColor.GRAY
                            + " Either you're missing something, or we are." ) );
            return;
        }

        call.getSender().sendMessage(
                Prism.messenger.playerHeaderMsg( "Showing " + results.getTotalResults() + " results. Page " + page
                        + " of " + results.getTotal_pages() ) );
        final List<Handler> paginated = results.getPaginatedActionResults();
        if( paginated == null || paginated.size() == 0 ) {
            call.getSender()
                    .sendMessage(
                            Prism.messenger
                                    .playerError( "Pagination can't find anything. Do you have the right page number?" ) );
            return;
        }

        // Show it!
        int result_count = results.getIndexOfFirstResult();
        for ( final Handler a : paginated ) {
            final ActionMessage am = new ActionMessage( a );
            if( results.getParameters().allowsNoRadius() || results.getParameters().hasFlag( Flag.EXTENDED )
                    || plugin.getConfig().getBoolean( "prism.messenger.always-show-extended" ) ) {
                am.showExtended();
            }
            am.setResultIndex( result_count );
            call.getSender().sendMessage( Prism.messenger.playerMsg( am.getMessage() ) );
            result_count++;
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}