package me.botsko.prism.parameters;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.utils.LevenshteinDistance;
import org.bukkit.command.CommandSender;

import java.util.ArrayList;
import java.util.regex.Pattern;

public class ActionParameter extends SimplePrismParameterHandler {

    /**
	 * 
	 */
    public ActionParameter() {
        super( "Action", Pattern.compile( "[~|!]?[\\w,-]+" ), "a" );
    }

    /**
	 * 
	 */
    @Override
    public void process(QueryParameters query, String alias, String input, CommandSender sender) {
        // Check match type
        MatchRule match = MatchRule.INCLUDE;
        if( input.startsWith( "!" ) ) {
            match = MatchRule.EXCLUDE;
        }

        final String[] actions = input.split( "," );
        if( actions.length > 0 ) {
            for ( final String action : actions ) {
                // Find all actions that match the action provided - whether the
                // full name or
                // short name.
                final ArrayList<ActionType> actionTypes = Prism.getActionRegistry().getActionsByShortname(
                        action.replace( "!", "" ) );
                if( !actionTypes.isEmpty() ) {
                    for ( final ActionType actionType : actionTypes ) {

                        // Ensure the action allows this process type
                        if( ( query.getProcessType().equals( PrismProcessType.ROLLBACK ) && !actionType.canRollback() )
                                || ( query.getProcessType().equals( PrismProcessType.RESTORE ) && !actionType
                                        .canRestore() ) ) {
                            // @todo this is important information but is too
                            // spammy with a:place, because vehicle-place
                            // doesn't support a rollback etc
                            // respond( sender,
                            // Prism.messenger.playerError("Ingoring action '"+actionType.getName()+"' because it doesn't support rollbacks.")
                            // );
                            continue;
                        }

                        query.addActionType( actionType.getName(), match );
                    }
                } else {
                    if( sender != null ) {
                        sender.sendMessage( Prism.messenger.playerError( "Ignoring action '" + action.replace( "!", "" )
                                + "' because it's unrecognized. Did you mean '"
                                + LevenshteinDistance.getClosestAction( action ) + "'? Type '/prism params' for help." ) );
                    }
                }
            }
            // If none were valid, we end here.
            if( query.getActionTypes().size() == 0 ) { throw new IllegalArgumentException(
                    "Action parameter value not recognized. Try /pr ? for help" ); }
        }
    }
}