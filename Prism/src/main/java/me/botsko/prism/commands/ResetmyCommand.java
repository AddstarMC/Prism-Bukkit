package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.settings.Settings;
import me.botsko.prism.text.ReplaceableTextComponent;
import me.botsko.prism.wands.Wand;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.Style;

import java.util.List;

public class ResetmyCommand extends AbstractCommand {

    private final Prism plugin;

    /**
     * Constructor.
     *
     * @param plugin Prism
     */
    public ResetmyCommand(Prism plugin) {
        this.plugin = plugin;
    }

    @Override
    public void handle(CallInfo call) {

        String setType = null;
        if (call.getArgs().length >= 2) {
            setType = call.getArg(1);
        }

        if (setType != null && !setType.equalsIgnoreCase("wand")) {
            Prism.messenger.sendMessage(call.getPlayer(),
                    Prism.messenger.playerError(Il8nHelper.getMessage("invalid-arguments")));
            return;
        }

        if (!plugin.getConfig().getBoolean("prism.wands.allow-user-override")) {
            Prism.messenger.sendMessage(call.getPlayer(),
                    Prism.messenger.playerError(Il8nHelper.getMessage("wand-personal-blocked")));
        }
        if (checkNoPermissions(call.getPlayer(), "prism.rollback", "prism.restore", "prism.wand.*",
                "prism.wand.inspect", "prism.wand.profile", "prism.wand.rollback",
                "prism.wand.restore")) {
            return;
        }
        // Disable any current wand
        if (Prism.playersWithActiveTools.containsKey(call.getPlayer().getName())) {
            final Wand oldwand = Prism.playersWithActiveTools.get(call.getPlayer().getName());
            oldwand.disable(call.getPlayer());
            Prism.playersWithActiveTools.remove(call.getPlayer().getName());
            Prism.messenger.sendMessage(call.getPlayer(), Prism.messenger
                    .playerHeaderMsg(ReplaceableTextComponent.builder("wand-current")
                            .replace("<status", Il8nHelper.getRawMessage("disabled"),
                                    Style.style(NamedTextColor.RED))
                            .build()));
        }

        Settings.deleteSetting("wand.item", call.getPlayer());
        Settings.deleteSetting("wand.mode", call.getPlayer());
        Prism.messenger.sendMessage(call.getPlayer(),
                Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("wand-reset")));
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}