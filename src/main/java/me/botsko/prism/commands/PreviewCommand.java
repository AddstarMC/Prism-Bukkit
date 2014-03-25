package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.*;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.MiscUtils;

import java.util.ArrayList;
import java.util.List;

public class PreviewCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    private final List<String> secondaries;

    /**
     * 
     * @param plugin
     * @return
     */
    public PreviewCommand(Prism plugin) {
        this.plugin = plugin;
        secondaries = new ArrayList<String>();
        secondaries.add( "apply" );
        secondaries.add( "cancel" );
        secondaries.add( "rollback" );
        secondaries.add( "restore" );
        secondaries.add( "rb" );
        secondaries.add( "rs" );
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(CallInfo call) {
        if( call.getArgs().length >= 2 ) {

            /**
             * Apply previous preview changes
             */
            if( call.getArg( 1 ).equalsIgnoreCase( "apply" ) ) {
                if( plugin.playerActivePreviews.containsKey( call.getPlayer().getName() ) ) {
                    final PreviewSession previewSession = plugin.playerActivePreviews.get( call.getPlayer().getName() );
                    previewSession.getPreviewer().apply_preview();
                    plugin.playerActivePreviews.remove( call.getPlayer().getName() );
                } else {
                    call.getPlayer().sendMessage( Prism.messenger.playerError( "You have no preview pending." ) );
                }
                return;
            }

            /**
             * Cancel preview
             */
            if( call.getArg( 1 ).equalsIgnoreCase( "cancel" ) ) {
                if( plugin.playerActivePreviews.containsKey( call.getPlayer().getName() ) ) {
                    final PreviewSession previewSession = plugin.playerActivePreviews.get( call.getPlayer().getName() );
                    previewSession.getPreviewer().cancel_preview();
                    plugin.playerActivePreviews.remove( call.getPlayer().getName() );
                } else {
                    call.getPlayer().sendMessage( Prism.messenger.playerError( "You have no preview pending." ) );
                }
                return;
            }

            // Ensure no current preview is waiting
            if( plugin.playerActivePreviews.containsKey( call.getPlayer().getName() ) ) {
                call.getPlayer()
                        .sendMessage(
                                Prism.messenger
                                        .playerError( "You have an existing preview pending. Please apply or cancel before moving on." ) );
                return;
            }

            /**
             * Begin a rollback or restore preview
             */
            if( call.getArg( 1 ).equalsIgnoreCase( "rollback" ) || call.getArg( 1 ).equalsIgnoreCase( "restore" )
                    || call.getArg( 1 ).equalsIgnoreCase( "rb" ) || call.getArg( 1 ).equalsIgnoreCase( "rs" ) ) {

                final QueryParameters parameters = PreprocessArgs.process( plugin, call.getPlayer(), call.getArgs(),
                        PrismProcessType.ROLLBACK, 2,
                        !plugin.getConfig().getBoolean( "prism.queries.never-use-defaults" ) );
                if( parameters == null ) { return; }
                parameters.setStringFromRawArgs( call.getArgs(), 1 );

                if( parameters.getActionTypes().containsKey( "world-edit" ) ) {
                    call.getPlayer()
                            .sendMessage(
                                    Prism.messenger
                                            .playerError( "Prism does not support previews for WorldEdit rollbacks/restores yet." ) );
                    return;
                }

                // determine if defaults were used
                final ArrayList<String> defaultsUsed = parameters.getDefaultsUsed();
                String defaultsReminder = "";
                if( !defaultsUsed.isEmpty() ) {
                    defaultsReminder += " using defaults:";
                    for ( final String d : defaultsUsed ) {
                        defaultsReminder += " " + d;
                    }
                }

                call.getPlayer().sendMessage(
                        Prism.messenger.playerSubduedHeaderMsg( "Preparing results..." + defaultsReminder ) );

                // Perform preview
                final ActionsQuery aq = new ActionsQuery( plugin );
                final QueryResult results = aq.lookup( parameters, call.getPlayer() );

                // Rollback
                if( call.getArg( 1 ).equalsIgnoreCase( "rollback" ) || call.getArg( 1 ).equalsIgnoreCase( "rb" ) ) {
                    parameters.setProcessType( PrismProcessType.ROLLBACK );
                    if( !results.getActionResults().isEmpty() ) {

                        call.getPlayer().sendMessage( Prism.messenger.playerHeaderMsg( "Beginning preview..." ) );

                        final Previewable rs = new Rollback( plugin, call.getPlayer(), results.getActionResults(),
                                parameters, new PrismApplierCallback() );
                        rs.preview();
                    } else {
                        call.getPlayer().sendMessage( Prism.messenger.playerError( "Nothing found to preview." ) );
                    }
                }
                // Restore
                if( call.getArg( 1 ).equalsIgnoreCase( "restore" ) || call.getArg( 1 ).equalsIgnoreCase( "rs" ) ) {
                    parameters.setProcessType( PrismProcessType.RESTORE );
                    if( !results.getActionResults().isEmpty() ) {

                        call.getPlayer().sendMessage( Prism.messenger.playerHeaderMsg( "Beginning preview..." ) );

                        final Previewable rs = new Restore( plugin, call.getPlayer(), results.getActionResults(),
                                parameters, new PrismApplierCallback() );
                        rs.preview();
                    } else {
                        call.getPlayer().sendMessage( Prism.messenger.playerError( "Nothing found to preview." ) );
                    }
                }
                return;
            }

            call.getPlayer().sendMessage( Prism.messenger.playerError( "Invalid command. Check /prism ? for help." ) );

        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        if( call.getArgs().length == 2 ) { return MiscUtils.getStartingWith( call.getArg( 1 ), secondaries ); }
        return PreprocessArgs.complete( call.getSender(), call.getArgs() );
    }
}