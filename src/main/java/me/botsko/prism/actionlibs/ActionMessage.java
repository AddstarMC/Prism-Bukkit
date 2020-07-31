package me.botsko.prism.actionlibs;

import me.botsko.prism.actions.Handler;
import me.botsko.prism.utils.block.Utilities;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.event.HoverEvent;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextColor;
import net.kyori.adventure.text.serializer.plain.PlainComponentSerializer;
import org.bukkit.Location;
import org.bukkit.Material;

import java.util.regex.Pattern;

public class ActionMessage {

    protected final Handler handler;

    private boolean showExtended = false;

    private int index = 0;

    public ActionMessage(Handler handler) {
        this.handler = handler;
    }

    public final int getIndex() {
        return index;
    }

    public void showExtended() {
        showExtended = true;
    }

    public void setResultIndex(int index) {
        this.index = index;
    }

    /**
     * Here, we don't use formatting or anything, we just use a regular message raw.
     * This will automatically show extended information, as this can be passed to a
     * paste service.
     *
     * @return String
     */
    public String getRawMessage() {
        String format1 = "<prefix> <handlerId> <target> <actor> <extendedInfo> <actorNice> <count>"
                + " <timeDiff> <location>";
        ActionType action = handler.getActionType();
        return PlainComponentSerializer.plain().serialize(getMainMessage(action, format1));
    }

    private TextComponent getMainMessage(ActionType action, String format1) {
        final TextColor highlight = NamedTextColor.DARK_AQUA;
        TextComponent out = TextComponent.builder()
                .content(format1)
                .build();
        return out.replaceFirst(Pattern.compile("<prefix>"), builder -> getPosNegPrefix())
                .replaceFirst(Pattern.compile("<index>"),
                        builder -> builder.content("[" + index + "] ")
                                .color(NamedTextColor.GRAY))
                .replaceFirst(Pattern.compile("<target>"),
                        builder -> TextComponent.builder().content(handler.getSourceName())
                                .color(highlight))
                .replaceFirst(Pattern.compile("<description>"),
                        builder -> TextComponent.builder().content(getDescription(action))
                                .color(NamedTextColor.WHITE))
                .replaceFirst(Pattern.compile("<actorNice>"), builder -> getActor(action, highlight))
                .replaceFirst(Pattern.compile("<actor>"),
                        builder -> TextComponent.builder().content(action.getName()))
                .replaceFirst(Pattern.compile("<extendedInfo>"),
                        builder -> TextComponent.builder().append(getExtendedInfo()))
                .replaceFirst(Pattern.compile("<timeDiff>"),
                        builder -> TextComponent.builder().append(getTimeDiff()))
                .replaceFirst(Pattern.compile("<count>"),
                        builder -> TextComponent.builder().append(getCount()))
                .replaceFirst(Pattern.compile("<actionType>"),
                        builder -> TextComponent.builder()
                                .content("(a:" + action.getShortName() + ")")
                                .color(NamedTextColor.GRAY))
                .replaceFirst(Pattern.compile("<handlerId>"),
                        builder -> TextComponent.of(handler.getId()).toBuilder()
                                .color(NamedTextColor.GRAY))
                .toBuilder()
                .hoverEvent(HoverEvent.showText(TextComponent.of("Click to teleport")))
                .clickEvent(ClickEvent.runCommand("/pr tp " + index))
                .build();
    }

    /**
     * Get the message.
     *
     * @return String[]
     */
    public TextComponent getMessage() {
        String format1 =
                "<prefix> <index> <target> <description> <actorNice> <extendedInfo> <count> <timeDiff> <actionType>";
        String format2 = "-<handlerId>- <dateTime> - <location>";
        ActionType action = handler.getActionType();
        TextComponent out = getMainMessage(action, format1);
        if (showExtended) {
            out = out.append(TextComponent.newline());
            TextComponent line2 = TextComponent.builder().content(format2).build()
                    .replaceFirst(Pattern.compile("<handlerId>"),
                            builder -> TextComponent.of(handler.getId()).toBuilder()
                                    .color(NamedTextColor.GRAY))
                    .replaceFirst(Pattern.compile("<dateTime>"),
                            builder -> TextComponent.builder()
                                    .content(handler.getDisplayDate() + " " + handler.getDisplayTime()))
                    .replaceFirst(Pattern.compile("<location>"),
                            builder -> TextComponent.builder().content(getFormattedLocation()));
            out = out.append(line2);
        }
        return out;
    }

    private String getFormattedLocation() {
        Location l = handler.getLoc();
        if (l.getWorld() != null) {
            return l.getWorld().getName() + " @ " + l.getBlockX() + " "
                    + l.getBlockY() + " " + l.getBlockZ();
        }
        return "INVALID";
    }


    private String getDescription(ActionType action) {
        String description = handler.getCustomDesc();
        if (description == null) {
            description = action.getNiceDescription();
        }
        return description;
    }

    private TextComponent getExtendedInfo() {
        if (showExtended && (handler.getMaterial() != Material.AIR)) {
            return TextComponent.of(handler.getMaterial() + Utilities.dataString(handler.getBlockData()));
        }
        return TextComponent.empty();
    }

    private TextComponent.Builder getActor(ActionType action, TextColor highlight) {
        String target = "unknown";
        if (action.getHandler() != null) {
            if (!handler.getNiceName().isEmpty()) {
                target = handler.getNiceName();
            }
        } else {
            // We should really improve this, but this saves me from having to
            // make
            // a custom handler.
            if (action.getName().equals("lava-bucket")) {
                target = "lava";
            } else if (action.getName().equals("water-bucket")) {
                target = "water";
            }
        }
        return TextComponent.builder()
                .content(target)
                .color(highlight);
    }

    private TextComponent getCount() {
        if (handler.getAggregateCount() > 1) {
            return TextComponent.of(" x" + handler.getAggregateCount());
        }
        return TextComponent.empty();
    }

    private TextComponent getTimeDiff() {
        // Time since
        if (!handler.getTimeSince().isEmpty()) {
            return TextComponent.builder().content(handler.getTimeSince()).color(NamedTextColor.WHITE).build();
        } else {
            return TextComponent.empty();
        }
    }

    private TextComponent.Builder getPosNegPrefix() {
        if (handler.getActionType().doesCreateBlock() || handler.getActionType().getName().equals("item-insert")
                || handler.getActionType().getName().equals("sign-change")) {
            return TextComponent.builder().content("+").color(NamedTextColor.GREEN);
        } else {
            return TextComponent.builder().content("-").color(NamedTextColor.RED);
        }
    }
}