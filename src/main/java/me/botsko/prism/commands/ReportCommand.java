package me.botsko.prism.commands;

import com.zaxxer.hikari.HikariDataSource;
import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.RecordingManager;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.database.ActionReportQuery;
import me.botsko.prism.database.BlockReportQuery;
import me.botsko.prism.measurement.QueueStats;
import me.botsko.prism.text.ReplaceableTextComponent;
import me.botsko.prism.utils.MiscUtils;
import net.kyori.adventure.identity.Identity;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.command.CommandSender;

import java.sql.Connection;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentSkipListMap;

public class ReportCommand extends AbstractCommand {

    private final Prism plugin;
    private final List<String> secondaries;
    private final List<String> sumTertiaries;

    /**
     * Constructor.
     *
     * @param plugin Prism
     */
    public ReportCommand(Prism plugin) {
        this.plugin = plugin;
        secondaries = new ArrayList<>();
        secondaries.add("queue");
        secondaries.add("db");
        secondaries.add("sum");
        sumTertiaries = new ArrayList<>();
        sumTertiaries.add("blocks");
        sumTertiaries.add("actions");
    }

    /**
     * Handle the command.
     */
    @Override
    public void handle(CallInfo call) {

        if (call.getArgs().length < 2) {
            Prism.messenger.sendMessage(call.getSender(),
                  Prism.messenger.playerError(Il8nHelper.getMessage("report-error")));
            return;
        }

        // /prism report queue
        if (call.getArg(1).equals("queue")) {
            queueReport(call.getSender());
        }

        // /prism report db
        if (call.getArg(1).equals("db")) {
            databaseReport(call.getSender());
        }

        // /prism report queue
        if (call.getArg(1).equals("sum")) {

            if (call.getArgs().length < 3) {
                Prism.messenger.sendMessage(call.getSender(),
                        Prism.messenger.playerError(Il8nHelper.getMessage("report-sum-error")));
                return;
            }

            if (call.getArgs().length < 4) {
                Prism.messenger.sendMessage(call.getSender(),
                        Prism.messenger.playerError(Il8nHelper.getMessage("report-player-error")));
                return;
            }

            if (call.getArg(2).equals("blocks")) {
                blockSumReports(call);
            }

            if (call.getArg(2).equals("actions")) {
                actionTypeCountReport(call);
            }
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        if (call.getArgs().length == 2) {
            return MiscUtils.getStartingWith(call.getArg(1), secondaries);
        }
        if (call.getArg(1).equals("sum")) {
            if (call.getArgs().length == 3) {
                return MiscUtils.getStartingWith(call.getArg(2), sumTertiaries);
            }
            return PreprocessArgs.complete(call.getSender(), call.getArgs());
        }
        return null;
    }

    private void queueReport(CommandSender sender) {

        Prism.messenger.sendMessage(sender,
                Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("report-queue-header")));

        Prism.messenger.sendMessage(sender,
              Prism.messenger.playerMsg(ReplaceableTextComponent.builder("report-actions-queue")
                    .replace("<size>", RecordingQueue.getQueueSize())
                    .build()));

        final ConcurrentSkipListMap<Long, QueueStats.TaskRunInfo> runs = plugin.queueStats.getRecentRunCounts();
        if (runs.size() > 0) {
            Prism.messenger.sendMessage(sender,
                    Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("report-queue-recent")));
            for (final Entry<Long, QueueStats.TaskRunInfo> entry : runs.entrySet()) {
                final String time = new SimpleDateFormat("HH:mm:ss").format(entry.getKey());
                Prism.messenger.sendMessage(sender,
                        Prism.messenger.playerMsg(Component.text()
                                .content(time + " ").color(NamedTextColor.GRAY)
                                .append(Component.text(entry.getValue().getRecords()))
                                .build()));
            }
        }
    }

    private void databaseReport(CommandSender sender) {

        Prism.messenger.sendMessage(sender, Prism.messenger.playerHeaderMsg(
                Il8nHelper.getMessage("report-database-header")));

        Prism.messenger.sendMessage(sender, Prism.messenger
              .playerMsg(ReplaceableTextComponent.builder("report-database-failureCount")
                    .replace("<count>", RecordingManager.failedDbConnectionCount)
                    .build()));
        Prism.messenger.sendMessage(sender,
              Prism.messenger.playerMsg(ReplaceableTextComponent.builder("report-actions-queue")
                    .replace("<size>", RecordingQueue.getQueueSize())
                    .build()));
        if (Prism.getPrismDataSource().getDataSource() instanceof HikariDataSource) {
            HikariDataSource ds = (HikariDataSource) Prism.getPrismDataSource().getDataSource();
            Prism.messenger.sendMessage(sender, Prism.messenger.playerMsg(ReplaceableTextComponent
                  .builder("report-hikari-props")
                  .replace("<total>", ds.getHikariPoolMXBean().getTotalConnections())
                  .replace("<activeConnections>", ds.getHikariPoolMXBean().getActiveConnections())
                  .replace("<idleConnections>", ds.getHikariPoolMXBean().getIdleConnections())
                  .replace("<minIdleConnections>", ds.getMinimumIdle())
                  .replace("<maxIdleConnections>", ds.getMaximumPoolSize())
                  .build()));
        }

        boolean recorderActive = checkRecorderActive(plugin);

        if (recorderActive) {
            Prism.messenger.sendMessage(sender,
                    Prism.messenger.playerSuccess(Il8nHelper.getMessage("report-recorder-running")));
        } else {
            Prism.messenger.sendMessage(sender,
                    Prism.messenger.playerError(Il8nHelper.getMessage("report-recorder-stopped")));
        }

        Prism.messenger.sendMessage(sender,
                Prism.messenger.playerSubduedHeaderMsg(Il8nHelper.getMessage("report-recorder-readiness")));

        try (Connection conn = Prism.getPrismDataSource().getConnection()) {
            if (conn == null) {
                Prism.messenger.sendMessage(sender,
                        Prism.messenger.playerError(Il8nHelper.getMessage("pool-no-valid")));
            } else if (conn.isClosed()) {
                Prism.messenger.sendMessage(sender,
                        Prism.messenger.playerError(Il8nHelper.getMessage("pool-connection-closed")));
            } else if (conn.isValid(5)) {
                Prism.messenger.sendMessage(sender,
                        Prism.messenger.playerSuccess(Il8nHelper.getMessage("pool-valid-connection")));
            }
        } catch (final SQLException e) {
            Prism.messenger.sendMessage(sender, Prism.messenger
                  .playerError(ReplaceableTextComponent.builder("exception-message")
                        .replace("<message>", e.getLocalizedMessage())
                        .build()));
            e.printStackTrace();
        }
    }


    private void blockSumReports(final CallInfo call) {

        // Process and validate all of the arguments
        final QueryParameters parameters = PreprocessArgs.process(plugin, call.getSender(), call.getArgs(),
              PrismProcessType.LOOKUP, 3, !plugin.getConfig().getBoolean("prism.queries.never-use-defaults"));
        if (parameters == null) {
            Prism.getAudiences().sender(call.getSender())
                    .sendMessage(Identity.nil(),
                          Prism.messenger.playerError(Il8nHelper.getMessage("report-player-error")));
            return;
        }
        // No actions
        if (checkParams(parameters, call)) {
            return;
        }

        final BlockReportQuery reportQuery = Prism.getPrismDataSource().createBlockReportQuery();
        reportQuery.setParameters(parameters);
        /*
          Run the lookup itself in an async task so the lookup query isn't done on the
          main thread
         */
        plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> reportQuery.report(call.getSender()));
    }

    private boolean checkParams(QueryParameters parameters, CallInfo call) {
        if (!parameters.getActionTypes().isEmpty()) {
            Prism.getAudiences().sender(call.getSender())
                    .sendMessage(Identity.nil(),
                            Prism.messenger.playerError(
                                    Il8nHelper.getMessage("report-actions-invalid")));
            return true;
        }
        // Verify single player name for now
        final Map<String, MatchRule> players = parameters.getPlayerNames();
        if (players.size() != 1) {
            Prism.messenger.sendMessage(call.getSender(),
                    Prism.messenger.playerError(Il8nHelper.getMessage("single-player-only")));
            return true;
        }
        return false;
    }

    private void actionTypeCountReport(final CallInfo call) {

        // Process and validate all of the arguments
        final QueryParameters parameters = PreprocessArgs.process(plugin, call.getSender(), call.getArgs(),
              PrismProcessType.LOOKUP, 3,
              !plugin.getConfig().getBoolean("prism.queries.never-use-defaults"));
        if (parameters == null) {
            return;
        }

        // No actions
        if (checkParams(parameters, call)) {
            return;
        }
        final ActionReportQuery reportQuery = Prism.getPrismDataSource().createActionReportQuery();
        reportQuery.setParameters(parameters);
        plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> reportQuery.report(call.getSender()));
    }
}