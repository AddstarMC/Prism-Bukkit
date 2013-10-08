package me.botsko.prism.actionlibs;

import me.botsko.prism.actions.Handler;

import org.bukkit.ChatColor;

public class ActionMessage {

	/**
	 * 
	 */
	protected Handler a;

	/**
	 * 
	 */
	private boolean showExtended = false;

	/**
	 * 
	 */
	private int index = 0;

	/**
	 * @param a
	 */
	public ActionMessage(Handler a) {
		this.a = a;
	}

	/**
	 * 
	 */
	public void showExtended() {
		showExtended = true;
	}

	/**
	 * @param index
	 */
	public void setResultIndex(int index) {
		this.index = index;
	}

	/**
	 * 
	 */
	public String[] getMessage() {

		String[] msg = new String[1];
		if (showExtended) {
			msg = new String[2];
		}

		ChatColor highlight = ChatColor.DARK_AQUA;

		String line1 = "";

		// +/-
		line1 += getPosNegPrefix();

		// Result index for teleporting
		if (index > 0) {
			line1 += ChatColor.GRAY + " [" + index + "] ";
		}

		// Who
		line1 += highlight + a.getPlayerName();

		// Description of event
		line1 += " " + ChatColor.WHITE + a.getType().getNiceDescription();
		if (a.getType().getHandler() != null) {
			line1 += " " + highlight + a.getNiceName();
		} else {
			// We should really improve this, but this saves me from having to make
			// a custom handler.
			if (a.getType().getName().equals("lava-bucket")) {
				line1 += " " + highlight + "lava";
			}
			else if (a.getType().getName().equals("water-bucket")) {
				line1 += " " + highlight + "water";
			}
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
		line1 += " " + ChatColor.GRAY + "(a:" + a.getType().getShortName() + ")";

		// Line 2
		String line2 = ChatColor.GRAY + " --";

		line2 += ChatColor.GRAY + " " + a.getId() + " - ";

		// Date & Time
		if (showExtended) {
			line2 += ChatColor.GRAY + a.getDisplayDate();
			line2 += " " + ChatColor.GRAY + a.getDisplayTime().toLowerCase();

			line2 += " - " + a.getWorldName() + " @ " + a.getX() + " " + a.getY() + " " + a.getZ() + " ";
		}

		msg[0] = line1;
		if (showExtended) {
			msg[1] = line2;
		}

		return msg;

	}

	/**
	 * @return
	 */
	protected String getPosNegPrefix() {

		if (a.getType().doesCreateBlock() || a.getType().getName().equals("item-insert") || a.getType().getName().equals("sign-change")) {
			return ChatColor.GREEN + " + " + ChatColor.WHITE;
		}
		else {
			return ChatColor.RED + " - " + ChatColor.WHITE;
		}
	}
}
