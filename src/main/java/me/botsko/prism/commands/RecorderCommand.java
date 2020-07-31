package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import net.kyori.adventure.text.TextComponent;

import java.sql.Connection;
import java.util.List;

public class RecorderCommand extends AbstractCommand {

    private final Prism plugin;

    /**
     * Constructor.
     * @param plugin Prism
     */
    public RecorderCommand(Prism plugin) {
        this.plugin = plugin;
    }

    @Override
    public void handle(final CallInfo call) {

        if (call.getArgs().length <= 1) {
            Prism.messenger.sendMessage(call.getSender(),
                    Prism.messenger.playerError(
                            TextComponent.of("Invalid command. Use /pr ? for help")));
            return;
        }

        boolean recorderActive = checkRecorderActive(plugin);

        // Allow for canceling recorders
        if (call.getArg(1).equals("cancel")) {
            if (recorderActive) {
                plugin.recordingTask.cancel();
                plugin.recordingTask = null;
                Prism.messenger.sendMessage(call.getSender(), Prism.messenger
                        .playerMsg("Current recording task has been canceled."));
                Prism.messenger.sendMessage(call.getSender(), Prism.messenger
                        .playerError(TextComponent.of("WARNING: Actions will collect until queue "
                                + "until recorder restarted manually.")));
            } else {
                Prism.messenger.sendMessage(call.getSender(),
                        Prism.messenger.playerError(
                                TextComponent.of("No recording task is currently running.")));
            }
            return;
        }

        // Allow for force-restarting recorders
        if (call.getArg(1).equals("start")) {
            if (recorderActive) {
                Prism.messenger.sendMessage(call.getSender(),
                        Prism.messenger.playerError(
                                TextComponent.of("Recording tasks are currently running. Cannot start.")));
            } else {

                // Run db tests...
                Prism.getAudiences().audience(call.getSender())
                        .sendMessage(Prism.messenger.playerMsg("Validating database connections..."));

                try (
                        Connection conn = Prism.getPrismDataSource().getConnection()
                ) {
                    if (conn == null || conn.isClosed()) {
                        Prism.getAudiences().audience(call.getSender())
                                .sendMessage(Prism.messenger.playerError(
                                        TextComponent.of("Valid database connection could not be found. "
                                                + "Check the db/console and try again.")));
                        return;
                    }

                    Prism.messenger.sendMessage(call.getSender(),
                            Prism.messenger.playerSuccess("Valid connection found. Yay!"));

                    Prism.messenger.sendMessage(call.getSender(),
                            Prism.messenger.playerMsg("Restarting recordering tasks..."));
                    plugin.actionRecorderTask();

                } catch (final Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}