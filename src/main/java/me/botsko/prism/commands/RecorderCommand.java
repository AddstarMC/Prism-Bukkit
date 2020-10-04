package me.botsko.prism.commands;

import me.botsko.prism.Il8n;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;

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
                    Prism.messenger.playerError(Il8n.getMessage("invalid-command")));
            return;
        }

        boolean recorderActive = checkRecorderActive(plugin);

        // Allow for canceling recorders
        if (call.getArg(1).equals("cancel")) {
            if (recorderActive) {
                plugin.recordingTask.cancel();
                plugin.recordingTask = null;
                Prism.messenger.sendMessage(call.getSender(), Prism.messenger
                        .playerMsg(Il8n.getMessage("recorder-stopped")));
                Prism.messenger.sendMessage(call.getSender(), Prism.messenger
                        .playerError(Il8n.getMessage("recorder-stopped-warn")));
            } else {
                Prism.messenger.sendMessage(call.getSender(),
                        Prism.messenger.playerError(Il8n.getMessage("report-recorder-stopped")));
            }
            return;
        }

        // Allow for force-restarting recorders
        if (call.getArg(1).equals("start")) {
            if (recorderActive) {
                Prism.messenger.sendMessage(call.getSender(),
                        Prism.messenger.playerError(Il8n.getMessage("report-already-running")));
            } else {

                // Run db tests...
                Prism.getAudiences().sender(call.getSender())
                        .sendMessage(Prism.messenger.playerMsg(Il8n.getMessage("database-validating")));

                try (
                        Connection conn = Prism.getPrismDataSource().getConnection()
                ) {
                    if (conn == null || conn.isClosed()) {
                        Prism.getAudiences().sender(call.getSender())
                                .sendMessage(Prism.messenger.playerError(Il8n.getMessage("no-valid-database")));
                        return;
                    }

                    Prism.messenger.sendMessage(call.getSender(),
                            Prism.messenger.playerSuccess(Il8n.getMessage("pool-valid-connection")));

                    Prism.messenger.sendMessage(call.getSender(),
                            Prism.messenger.playerMsg(Il8n.getMessage("recorder-restarting")));
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