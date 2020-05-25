package me.botsko.prism.actionlibs;

import me.botsko.prism.actions.Handler;
import me.botsko.prism.utils.block.Utilities;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;

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
        final StringBuilder msg = new StringBuilder();
        ActionType action = handler.getActionType();

        msg.append((action.doesCreateBlock() || action.getName().equals("item-insert")
                || action.getName().equals("sign-change")) ? "+" : "-");
        msg.append(" #").append(handler.getId());
        msg.append(" ").append(handler.getSourceName());
        msg.append(" ").append(action.getName());
        msg.append(" ").append(handler.getMaterial());
        msg.append(Utilities.dataString(handler.getBlockData()));

        if (action.getHandler() != null) {
            if (!handler.getNiceName().isEmpty()) {
                msg.append(" (").append(handler.getNiceName()).append(")");
            }
        } else {
            // We should really improve this, but this saves me from having to
            // make
            // a custom handler.
            if (action.getName().equals("lava-bucket")) {
                msg.append(" (lava)");
            } else if (action.getName().equals("water-bucket")) {
                msg.append(" (water)");
            }
        }
        if (handler.getAggregateCount() > 1) {
            msg.append(" x").append(handler.getAggregateCount());
        }
        msg.append(" ").append(handler.getDisplayDate());
        msg.append(" ").append(handler.getDisplayTime().toLowerCase());
        Location l = handler.getLoc();
        msg.append(" - ")
                .append(l.getWorld().getName())
                .append(" @ ")
                .append(l.getBlockX()).append(" ")
                .append(l.getBlockY()).append(" ")
                .append(l.getBlockZ());
        return msg.toString();
    }

    /**
     * Get the message.
     *
     * @return String[]
     */
    public String[] getMessage() {

        String[] msg = new String[1];
        if (showExtended) {
            msg = new String[2];
        }

        final ChatColor highlight = ChatColor.DARK_AQUA;

        String line1 = "";

        // +/-
        line1 += getPosNegPrefix();

        // Result index for teleporting
        if (index > 0) {
            line1 += ChatColor.GRAY + "[" + index + "] ";
        }

        // Who
        line1 += highlight + handler.getSourceName();

        String description = handler.getCustomDesc();
        ActionType action = handler.getActionType();

        if (description == null) {
            description = action.getNiceDescription();
        }

        // Description of event
        line1 += " " + ChatColor.WHITE + description;
        if (action.getHandler() != null) {
            if (!handler.getNiceName().isEmpty()) {
                line1 += " " + highlight + handler.getNiceName();
            }
        } else {
            // We should really improve this, but this saves me from having to
            // make
            // a custom handler.
            if (action.getName().equals("lava-bucket")) {
                line1 += " " + highlight + "lava";
            } else if (action.getName().equals("water-bucket")) {
                line1 += " " + highlight + "water";
            }
        }

        if (showExtended && (handler.getMaterial() != Material.AIR)) {
            line1 += " " + handler.getMaterial() + Utilities.dataString(handler.getBlockData());
        }

        // Aggregate count
        if (handler.getAggregateCount() > 1) {
            line1 += ChatColor.GREEN + " x" + handler.getAggregateCount();
        }

        // Time since
        if (!handler.getTimeSince().isEmpty()) {
            line1 += ChatColor.WHITE + " " + handler.getTimeSince();
        }

        // Action type reminder
        line1 += " " + ChatColor.GRAY + "(a:" + action.getShortName() + ")";

        if (showExtended) {
            line1 += "\n";

            // Line 2
            String line2 = ChatColor.GRAY + " - " + handler.getId() + " - ";

            // Date & Time
            line2 += handler.getDisplayDate();
            line2 += " " + handler.getDisplayTime().toLowerCase();

            // Location
            Location l = handler.getLoc();
            line2 += " - " + l.getWorld().getName() + " @ " + l.getBlockX() + " "
                    + l.getBlockY() + " " + l.getBlockZ() + " ";

            msg[1] = line2;
        }

        msg[0] = line1;

        return msg;

    }

    private String getPosNegPrefix() {

        if (handler.getActionType().doesCreateBlock() || handler.getActionType().getName().equals("item-insert")
                || handler.getActionType().getName().equals("sign-change")) {
            return ChatColor.GREEN + " + " + ChatColor.WHITE;
        } else {
            return ChatColor.RED + " - " + ChatColor.WHITE;
        }
    }
}