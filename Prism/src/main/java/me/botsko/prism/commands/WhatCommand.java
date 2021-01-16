package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Executor;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.ItemUtils;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.inventory.ItemStack;
import org.bukkit.plugin.Plugin;

import java.util.List;

public class WhatCommand extends Executor {

    /**
     * Constructor.
     *
     * @param prism Plugin
     */
    public WhatCommand(Plugin prism) {
        super(prism, "command", "prism");
        setupCommands();
    }

    private void setupCommands() {
        addSub("what", "prism.what").setHandler(new SubHandler() {
            @Override
            public void handle(CallInfo call) {
                final ItemStack item = call.getPlayer().getInventory().getItemInMainHand();
                Prism.messenger.sendMessage(call.getPlayer(),
                        Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("what-header", ":")));
                // TODO: Better material formatting
                TextComponent out = Component.text()
                        .append(Il8nHelper.getMessage("what-item-name", ": ").color(NamedTextColor.DARK_AQUA))
                        .append(Component.text(item.getType().toString().toLowerCase()))
                        .append(Component.newline())
                        .append(Il8nHelper.getMessage("what-alias", ": ").color(NamedTextColor.DARK_AQUA))
                        .append(Component.text(Prism.getItems().getAlias(item.getType(), null)))
                        .append(Component.newline())
                        .append(Il8nHelper.getMessage("what-id", ": "))
                        .append(Component.text(item.getType().toString()))
                        .append(Component.newline())
                        .append(Il8nHelper.getMessage("what-display-name", ": ").color(NamedTextColor.DARK_AQUA))
                        .append(Component.text(ItemUtils.getItemFullNiceName(item)))
                        .colorIfAbsent(NamedTextColor.WHITE)
                        .build();
                Prism.messenger.sendMessage(call.getPlayer(),out);
            }

            @Override
            public List<String> handleComplete(CallInfo call) {
                return null;
            }

            @Override
            public String[] getHelp() {
                return new String[]{Il8nHelper.getRawMessage("help-what")};
            }

            @Override
            public String getRef() {
                return ".html";
            }
        });
    }
}
