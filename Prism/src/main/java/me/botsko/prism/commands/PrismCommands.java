package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Executor;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.plugin.Plugin;

import java.util.List;

public class PrismCommands extends Executor {

    /**
     * Constructor.
     *
     * @param prism Plugin.
     */
    public PrismCommands(Plugin prism, boolean failed) {
        super(prism, "subcommand", "prism");
        setupCommands(failed);
    }

    private void setupCommands(boolean failed) {
        final Prism prism = (Prism) plugin;
        addSub(new String[]{"about", "default"}, "prism.help").allowConsole().setHandler(new AboutCommand(prism));
        addSub("debug", "prism.debug").allowConsole().setHandler(new DebugCommand());
        addSub(new String[]{"help", "?"}, "prism.help").allowConsole().setHandler(new HelpCommand(failed));
        addSub("flags", "prism.help").allowConsole().setHandler(new FlagsCommand());
        addSub("params", "prism.help").allowConsole().setHandler(new ParamsCommand());
        addSub("actions", "prism.help").allowConsole().setHandler(new ActionsCommand());
        addSub("settings", "prism.settings").allowConsole().setHandler(new SettingCommands());
        addSub("reload", "prism.reload").allowConsole().setHandler(new SubHandler() {

            @Override
            public void handle(CallInfo call) {
                prism.reloadConfig();
                prism.loadConfig();
                Prism.messenger.sendMessage(call.getSender(),
                        Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("prism-reload-success")));
                if (failed) {
                    Prism.messenger.sendMessage(call.getSender(),
                            Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("prism-reload-failed")));
                }
            }

            @Override
            public List<String> handleComplete(CallInfo call) {
                return null;
            }

            @Override
            public String[] getHelp() {
                return new String[]{Il8nHelper.getRawMessage("help-reload")};
            }

            @Override
            public String getRef() {
                return "";
            }
        });
        if (failed) {
            return;
        }
        addSub(new String[]{"lookup", "l"}, "prism.lookup").allowConsole().setMinArgs(1)
                .setHandler(new LookupCommand(prism));
        addSub("near", "prism.lookup").setHandler(new NearCommand(prism));
        addSub(new String[]{"page", "pg"}, new String[]{"prism.lookup.paginate", "prism.lookup"}).allowConsole()
                .setMinArgs(1).setHandler(new PageCommand(prism));
        addSub(new String[]{"wand", "w", "i", "inspect"},
                new String[]{"prism.rollback", "prism.restore", "prism.lookup", "prism.wand.inspect",
                      "prism.wand.profile", "prism.wand.rollback", "prism.wand.restore"})
                .setHandler(new WandCommand(prism));
        addSub(new String[]{"setmy"}, new String[]{"prism.setmy.wand"}).setHandler(new SetmyCommand(prism));
        addSub(new String[]{"resetmy"}, new String[]{"prism.setmy.wand"}).setHandler(new ResetmyCommand(prism));
        addSub("tp", "prism.tp").setMinArgs(1).setHandler(new TeleportCommand(prism));
        addSub("ex", "prism.extinguish").setHandler(new ExtinguishCommand(prism));
        addSub("drain", "prism.drain").setHandler(new DrainCommand(prism));
        addSub(new String[]{"preview", "pv"}, "prism.preview").setMinArgs(1).setHandler(new PreviewCommand(prism));
        addSub(new String[]{"report", "rp"}, "prism.report").allowConsole().setHandler(new ReportCommand(prism));
        addSub(new String[]{"rollback", "rb"}, "prism.rollback").allowConsole().setMinArgs(1)
                .setHandler(new RollbackCommand(prism));
        addSub(new String[]{"restore", "rs"}, "prism.restore").allowConsole().setMinArgs(1)
                .setHandler(new RestoreCommand(prism));
        addSub(new String[]{"delete", "purge"}, "prism.delete").allowConsole().setHandler(new DeleteCommand(prism));
        addSub("recorder", "prism.recorder").allowConsole().setHandler(new RecorderCommand(prism));
        addSub("undo", "prism.rollback").setHandler(new UndoCommand(prism));
        addSub(new String[]{"view", "v"}, "prism.view").setMinArgs(1).setHandler(new ViewCommand(prism));
        addSub("purge", "prism.purge").allowConsole().setHandler(new PurgeCommand(prism));
    }

}
