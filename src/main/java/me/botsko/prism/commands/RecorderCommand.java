package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

import java.sql.Connection;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.scheduler.BukkitScheduler;

public class RecorderCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     * @return
     */
    public RecorderCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(final CallInfo call) {

        if( call.getArgs().length <= 1 ) {
            call.getSender().sendMessage( Prism.messenger.playerError( "Invalid command. Use /pr ? for help" ) );
            return;
        }

        boolean recorderActive = false;
        if( plugin.recordingTask != null ) {
            final int taskId = plugin.recordingTask.getTaskId();
            final BukkitScheduler scheduler = Bukkit.getScheduler();
            if( scheduler.isCurrentlyRunning( taskId ) || scheduler.isQueued( taskId ) ) {
                recorderActive = true;
            }
        }

        // Allow for canceling recorders
        if( call.getArg( 1 ).equals( "cancel" ) ) {
            if( recorderActive ) {
                plugin.recordingTask.cancel();
                plugin.recordingTask = null;
                call.getSender().sendMessage( Prism.messenger.playerMsg( "Current recording task has been canceled." ) );
                call.getSender()
                        .sendMessage(
                                Prism.messenger
                                        .playerError( "WARNING: Actions will collect until queue until recorder restarted manually." ) );
            } else {
                call.getSender().sendMessage( Prism.messenger.playerError( "No recording task is currently running." ) );
            }
            return;
        }

        // Allow for force-restarting recorders
        if( call.getArg( 1 ).equals( "start" ) ) {
            if( recorderActive ) {
                call.getSender().sendMessage(
                        Prism.messenger.playerError( "Recording tasks are currently running. Cannot start." ) );
            } else {

                // Run db tests...
                call.getSender().sendMessage( Prism.messenger.playerMsg( "Validating database connections..." ) );

                // Attempt to get db
                Connection conn = null;
                try {

                    conn = Prism.dbc();
                    if( conn == null || conn.isClosed() ) {
                        call.getSender()
                                .sendMessage(
                                        Prism.messenger
                                                .playerError( "Valid database connection could not be found. Check the db/console and try again." ) );
                        return;
                    }

                    call.getSender().sendMessage( Prism.messenger.playerSuccess( "Valid connection found. Yay!" ) );

                    call.getSender().sendMessage( Prism.messenger.playerMsg( "Restarting recordering tasks..." ) );
                    plugin.actionRecorderTask();

                } catch ( final Exception e ) {
                    e.printStackTrace();
                } finally {
                    if( conn != null )
                        try {
                            conn.close();
                        } catch ( final Exception ignored ) {};
                }
            }
            return;
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}