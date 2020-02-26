package me.botsko.prism.actionlibs;

import me.botsko.prism.actions.Handler;
import me.botsko.prism.utils.BlockUtils;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;

public class ActionMessage {

	/**
	 *
	 */
	protected final Handler a;

	/**
	 *
	 */
	private boolean showExtended = false;
	/**
	 *
	 */
	private int index = 0;

	/**
	 * @param a Handler
	 */
	public ActionMessage(Handler a) {
		this.a = a;
	}

	public final int getIndex() {
		return index;
	}

	/**
	 *
	 */
	public void showExtended() {
		showExtended = true;
	}

	/**
	 * @param index integer
	 */
	public void setResultIndex(int index) {
		this.index = index;
	}

	/**
	 * Here, we don't use formatting or anything, we just use a regular message raw.
	 * <p>
	 * This will automatically show extended information, as this can be passed to a
	 * pastebin service.
	 *
	 * @return String
	 */
	public String getRawMessage() {
		final StringBuilder msg = new StringBuilder();
		ActionType action = a.getActionType();

		msg.append((action.doesCreateBlock() || action.getName().equals("item-insert")
			|| action.getName().equals("sign-change")) ? "+" : "-");
		msg.append(" #").append(a.getId());
		msg.append(" ").append(a.getSourceName());
		msg.append(" ").append(action.getName());
		msg.append(" ").append(a.getMaterial());
		msg.append(BlockUtils.dataString(a.getBlockData()));

		if (action.getHandler() != null) {
			if (!a.getNiceName().isEmpty())
				msg.append(" (").append(a.getNiceName()).append(")");
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
		if (a.getAggregateCount() > 1) {
			msg.append(" x").append(a.getAggregateCount());
		}
		msg.append(" ").append(a.getDisplayDate());
		msg.append(" ").append(a.getDisplayTime().toLowerCase());
		Location l = a.getLoc();
		msg.append(" - ").append(l.getWorld().getName()).append(" @ ").append(l.getBlockX()).append(" ").append(l.getBlockY()).append(" ").append(l.getBlockZ());
		return msg.toString();
	}

	/**
	 *
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
		line1 += highlight + a.getSourceName();

		String description = a.getCustomDesc();
		ActionType action = a.getActionType();

		if (description == null)
			description = action.getNiceDescription();

		// Description of event
		line1 += " " + ChatColor.WHITE + description;
		if (action.getHandler() != null) {
			if (!a.getNiceName().isEmpty())
				line1 += " " + highlight + a.getNiceName();
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

		if (showExtended && (a.getMaterial() != Material.AIR)) {
			line1 += " " + a.getMaterial() + BlockUtils.dataString(a.getBlockData());
		}

		// Aggregate count
		if (a.getAggregateCount() > 1) {
			line1 += ChatColor.GREEN + " x" + a.getAggregateCount();
		}

		// Time since
		if (!a.getTimeSince().isEmpty()) {
			line1 += ChatColor.WHITE + " " + a.getTimeSince();
		}

		// Action type reminder
		line1 += " " + ChatColor.GRAY + "(a:" + action.getShortName() + ")";

		if (showExtended) {
			line1 += "\n";

			// Line 2
			String line2 = ChatColor.GRAY + " - " + a.getId() + " - ";

			// Date & Time
			line2 += a.getDisplayDate();
			line2 += " " + a.getDisplayTime().toLowerCase();

			// Location
			Location l = a.getLoc();
			line2 += " - " + l.getWorld().getName() + " @ " + l.getBlockX() + " " + l.getBlockY() + " " + l.getBlockZ() + " ";

			msg[1] = line2;
		}

		msg[0] = line1;

		return msg;

	}

	/**
	 * @return String
	 */
	protected String getPosNegPrefix() {

		if (a.getActionType().doesCreateBlock() || a.getActionType().getName().equals("item-insert")
			|| a.getActionType().getName().equals("sign-change")) {
			return ChatColor.GREEN + " + " + ChatColor.WHITE;
		} else {
			return ChatColor.RED + " - " + ChatColor.WHITE;
		}
	}
}