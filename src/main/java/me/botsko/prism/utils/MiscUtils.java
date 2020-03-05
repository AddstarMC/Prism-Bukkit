package me.botsko.prism.utils;

import com.google.common.base.CaseFormat;
import io.papermc.lib.PaperLib;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.PrismProcessType;
import net.md_5.bungee.api.ChatColor;
import net.md_5.bungee.api.chat.BaseComponent;
import net.md_5.bungee.api.chat.ClickEvent;
import net.md_5.bungee.api.chat.HoverEvent;
import net.md_5.bungee.api.chat.TextComponent;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.kitteh.pastegg.Paste;
import org.kitteh.pastegg.PasteBuilder;
import org.kitteh.pastegg.PasteContent;
import org.kitteh.pastegg.PasteFile;
import org.kitteh.pastegg.Visibility;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MiscUtils {

    /**
     * Placing this here so it's easier to share the logic.
     *
     * @param player        Player
     * @param desiredRadius integer
     * @param processType   {@link PrismProcessType}
     * @param config        {@link FileConfiguration}
     * @return integer
     */
    public static int clampRadius(Player player, int desiredRadius, PrismProcessType processType,
                                  FileConfiguration config) {

        if (desiredRadius <= 0) {
            return config.getInt("prism.near.default-radius");
        }

        // Safety checks for max lookup radius
        int maxLookupRadius = config.getInt("prism.queries.max-lookup-radius");
        if (maxLookupRadius <= 0) {
            maxLookupRadius = 5;
            Prism.log("Max lookup radius may not be lower than one. Using safe inputue of five.");
        }

        // Safety checks for max applier radius
        int maxApplierRadius = config.getInt("prism.queries.max-applier-radius");
        if (maxApplierRadius <= 0) {
            maxApplierRadius = 5;
            Prism.log("Max applier radius may not be lower than one. Using safe inputue of five.");
        }

        // Does the radius exceed the configured max?
        if (processType.equals(PrismProcessType.LOOKUP) && desiredRadius > maxLookupRadius) {
            // If player does not have permission to override the max
            if (player != null && !player.hasPermission("prism.override-max-lookup-radius")) {
                return maxLookupRadius;
            }
            // Otherwise non-player
            return desiredRadius;
        } else if (!processType.equals(PrismProcessType.LOOKUP) && desiredRadius
                > maxApplierRadius) {
            // If player does not have permission to override the max
            if (player != null && !player.hasPermission("prism.override-max-applier-radius")) {
                return maxApplierRadius;
            }
            // Otherwise non-player
            return desiredRadius;
        } else {
            // Otherwise, the radius is valid and is not exceeding max
            return desiredRadius;
        }
    }

    /**
     * Get an enum that extends T as a fall back class.
     *
     * @param from     String
     * @param fallback the Class to fallback too
     * @param <T>      THe Class extending Enum
     * @return T
     */
    @SuppressWarnings("unchecked")
    public static <T extends Enum<T>> T getEnum(String from, T fallback) {
        if (from != null) {
            try {
                return (T) Enum.valueOf(fallback.getClass(), from.toUpperCase());
            } catch (IllegalArgumentException e) {
                Prism.debug(e.getMessage());
            }
        }
        return fallback;
    }

    public static String niceName(String in) {
        String[] parts = in.replace('_', ' ').trim().split("", 2);
        return parts[0].toUpperCase() + parts[1].toLowerCase();
    }

    /**
     * Paste results and return a web address.
     * Async Method.
     *
     * @param sender  CommandSender.
     * @param results String
     */
    public static void paste_results(CommandSender sender, String results) {

        final String prismWebUrl = "https://paste.gg/";

        if (!Prism.getInstance().getConfig().getBoolean("prism.paste.enable")) {
            sender.sendMessage(Prism.messenger.playerError(
                    "Paste.gg support is currently disabled by config."));
        }

        int expire = 60 * 60 * 1000;
        PasteFile file = new PasteFile("Prism Result",
                new PasteContent(PasteContent.ContentType.TEXT, results));

        final PasteBuilder.PasteResult result
                = new PasteBuilder().name("Prism Results")
                .expireIn(expire)
                .addFile(file)
                .visibility(Visibility.UNLISTED)
                .build();
        if (result.getPaste().isPresent()) {
            Paste paste = result.getPaste().get();
            sender.sendMessage(Prism.messenger.playerSuccess("Successfully pasted results: "
                    + prismWebUrl
                    + paste.getId()));
        } else {
            sender.sendMessage(Prism.messenger.playerError(
                    "Unable to paste results (" + ChatColor.YELLOW + result.getMessage()
                            + ChatColor.RED + ")."));

        }

    }

    /**
     * Send clickable record.
     *
     * @param a      ActionMessage
     * @param player CommandSender
     */
    public static void sendClickableTpRecord(ActionMessage a, CommandSender player) {
        boolean isSpigot = PaperLib.isSpigot();
        boolean isPaper = PaperLib.isPaper();

        if (isPaper || isSpigot) {
            String[] message = Prism.messenger.playerMsg(a.getMessage());
            //line 1 holds the index so we set that as the highlighted for command click
            final List<BaseComponent> toSend = new ArrayList<>();
            int i = 0;
            for (String m : message) {
                BaseComponent[] text = TextComponent.fromLegacyText(
                        (player instanceof Player) ? m : m.replace("\n", ""));
                if (i == 0) {
                    Arrays.asList(text).forEach(baseComponent -> {
                        baseComponent.setHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT,
                                new TextComponent[]{new TextComponent("Click to teleport")}));
                        baseComponent.setClickEvent(
                                new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/pr tp "
                                        + a.getIndex()));
                        toSend.add(baseComponent);
                    });
                } else {
                    toSend.addAll(Arrays.asList(text));
                }
                i++;
            }
            BaseComponent[] finalMessage = new BaseComponent[toSend.size()];
            toSend.toArray(finalMessage);
            if (PaperLib.isPaper()) {
                player.sendMessage(finalMessage);
            } else {
                player.spigot().sendMessage(finalMessage);
            }
        } else {
            player.sendMessage(Prism.messenger.playerMsg(a.getMessage()));
        }
    }

    /**
     * Send page buttons.
     *
     * @param results Results
     * @param player  Player
     */
    public static void sendPageButtons(QueryResult results, CommandSender player) {
        if (player instanceof Player) {
            if (PaperLib.isPaper()) {
                if (results.getPage() == 1) {
                    if (results.getTotal_pages() > 1) {
                        player.sendMessage(MiscUtils.getNextButton());
                    }
                } else if (results.getPage() < results.getTotal_pages()) {
                    player.sendMessage(MiscUtils.getPrevNextButtons());
                } else if (results.getPage() == results.getTotal_pages()) {
                    player.sendMessage(MiscUtils.getPreviousButton());
                }
            } else {
                if (results.getPage() == 1) {
                    if (results.getTotal_pages() > 1) {
                        player.spigot().sendMessage(MiscUtils.getNextButton());
                    }
                } else if (results.getPage() < results.getTotal_pages()) {
                    player.spigot().sendMessage(MiscUtils.getPrevNextButtons());
                } else if (results.getPage() == results.getTotal_pages()) {
                    player.spigot().sendMessage(MiscUtils.getPreviousButton());
                }
            }
        }
    }

    /**
     * Gets a list of strings starting with.
     *
     * @param start         String
     * @param options       Options
     * @param caseSensitive if case sensitive
     * @return List of Strings
     */
    public static List<String> getStartingWith(final String start, Iterable<String> options,
                                               boolean caseSensitive) {
        final List<String> result = new ArrayList<>();
        if (caseSensitive) {
            for (final String option : options) {
                if (option.startsWith(start)) {
                    result.add(option);
                }
            }
        } else {
            String caseStart = start.toLowerCase();
            for (final String option : options) {
                if (option.toLowerCase().startsWith(caseStart)) {
                    result.add(option);
                }
            }
        }

        return result;
    }

    public static List<String> getStartingWith(String arg, Iterable<String> options) {
        return getStartingWith(arg, options, true);
    }

    /**
     * Send alert.
     *
     * @param msg      the message
     * @param commands the commands
     */
    public static void dispatchAlert(String msg, List<String> commands) {
        String colorized = TypeUtils.colorize(msg);
        String stripped = ChatColor.stripColor(colorized);
        for (String command : commands) {
            if ("examplecommand <alert>".equals(command)) {
                continue;
            }
            String processedCommand = command.replace("<alert>", stripped);
            Bukkit.dispatchCommand(Bukkit.getConsoleSender(), processedCommand);
        }
    }

    /**
     * FInd a nice name for entity.
     *
     * @param entity Entity
     * @return SDtring
     */
    public static String getEntityName(Entity entity) {
        if (entity == null) {
            return "unknown";
        }
        if (entity.getType() == EntityType.PLAYER) {
            return entity.getName();
        }
        return CaseFormat.UPPER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, entity.getType().name());
    }

    /**
     * Prev Button.
     *
     * @return TextComponent
     */
    public static TextComponent getPreviousButton() {
        TextComponent textComponent = new TextComponent(" [<< Prev]");
        textComponent.setColor(ChatColor.GRAY);
        textComponent.setHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT,
                new TextComponent[]{new TextComponent("Click to view the previous page")}));
        textComponent.setClickEvent(
                new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/pr pg p"));
        return textComponent;

    }

    /**
     * NExt button.
     *
     * @return TextComponent
     */
    public static TextComponent getNextButton() {
        TextComponent textComponent = new TextComponent("           ");
        textComponent.setColor(ChatColor.GRAY);
        textComponent.addExtra(getNextButtonComponent());
        return textComponent;
    }

    /**
     * Next Button.
     *
     * @return BaseComponent.
     */
    private static BaseComponent getNextButtonComponent() {
        TextComponent textComponent = new TextComponent("[Next >>]");
        textComponent.setHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT,
                new TextComponent[]{new TextComponent("Click to view the next page")}));
        textComponent.setClickEvent(
                new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/pr pg n"));
        return textComponent;
    }

    /**
     * Prev Button.
     *
     * @return BaseComponent.
     */
    public static BaseComponent getPrevNextButtons() {
        TextComponent textComponent = new TextComponent();
        textComponent.setColor(ChatColor.GRAY);
        textComponent.addExtra(getPreviousButton());
        textComponent.addExtra(" | ");
        textComponent.addExtra(getNextButtonComponent());
        return textComponent;
    }
}