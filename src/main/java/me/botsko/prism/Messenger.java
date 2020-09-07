package me.botsko.prism;

import net.kyori.adventure.platform.AudienceProvider;
import net.kyori.adventure.platform.bukkit.BukkitAudiences;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextColor;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;

public class Messenger {


    private final AudienceProvider audienceProvider;
    private final String pluginName;
    private static final TextColor defaultColor = TextColor.of(0xb5bcc7);
    private static final TextColor headerColor = TextColor.of(0xb597ba);
    private static final TextColor error = TextColor.of(0x6e1017);
    private static final TextColor success = TextColor.of(0x4fab55);

    /**
     * Build the class.
     *
     * @param pluginName String
     */
    @SuppressWarnings("WeakerAccess")
    public Messenger(String pluginName, AudienceProvider provider) {
        this.pluginName = pluginName;
        this.audienceProvider = provider;
    }

    public void sendMessage(CommandSender sender, Component message) {
        if (sender instanceof ConsoleCommandSender) {
            audienceProvider.console().sendMessage(message);
        } else {
            ((BukkitAudiences) audienceProvider).audience(sender).sendMessage(message.colorIfAbsent(defaultColor));
        }
    }

    /**
     * Get the header.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public TextComponent playerHeaderMsg(Component msg) {
        if (msg != null) {
            return TextComponent.builder()
                    .content(pluginName + " ")
                    .color(headerColor)
                    .append(msg.colorIfAbsent(NamedTextColor.WHITE))
                    .build();
        }
        return TextComponent.empty();
    }

    @Deprecated
    public TextComponent playerHeaderMsg(String msg) {
        return this.playerHeaderMsg(LegacyComponentSerializer.legacySection().deserialize(msg));
    }

    /**
     * Get the subdued header.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public TextComponent playerSubduedHeaderMsg(Component msg) {
        if (msg != null) {
            return TextComponent.builder()
                    .content(pluginName + " ")
                    .color(headerColor)
                    .append(msg.colorIfAbsent(defaultColor))
                    .build();
        }
        return TextComponent.empty();
    }

    @Deprecated
    public TextComponent playerSubduedHeaderMsg(String msg) {
        return playerSubduedHeaderMsg(LegacyComponentSerializer.legacySection().deserialize(msg));
    }

    /**
     * Get the message.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    @Deprecated
    public Component playerMsg(String msg) {
        if (msg != null) {
            Component component = LegacyComponentSerializer.legacySection().deserialize(msg);
            return playerMsg(component);
        }
        return TextComponent.empty();
    }

    /**
     * Get the message colored white by default.
     *
     * @param msg TextComponent
     * @return TextComponent
     */
    public Component playerMsg(Component msg) {
        if (msg != null) {
            return msg.colorIfAbsent(defaultColor);
        }
        return TextComponent.empty();
    }

    /**
     * Get the help message.
     *
     * @param cmd  cmd to get help.
     * @param help - a message.
     * @return String.
     */
    public TextComponent playerHelp(String cmd, String help) {
        return TextComponent.builder()
                .content("/prism ").color(defaultColor)
                .build()
                .append(TextComponent.of(cmd).color(headerColor)
                        .append(TextComponent.of(" - " + help).color(NamedTextColor.WHITE)));
    }

    /**
     * Get the error message.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public TextComponent playerError(Component msg) {
        return TextComponent.builder()
                .content(pluginName + " ")
                .color(headerColor)
                .append(msg.colorIfAbsent(error))
                .build();
    }

    public TextComponent playerError(String msg) {
        return playerError(TextComponent.of(msg));
    }

    /**
     * Get the Success message.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public TextComponent playerSuccess(String msg) {
        if (msg != null) {
            return playerSuccess(TextComponent.of(msg));
        }
        return TextComponent.empty();
    }

    /**
     * Get the Success message.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public TextComponent playerSuccess(TextComponent msg) {
        if (msg != null) {
            return TextComponent.builder()
                    .content(pluginName + " ")
                    .color(headerColor)
                    .append(msg.colorIfAbsent(success))
                    .build();
        }
        return TextComponent.empty();
    }

}