package me.botsko.prism.actionlibs;

import me.botsko.prism.actions.Handler;

import org.bukkit.ChatColor;

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
     * 
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
     * 
     * @param index
     */
    public void setResultIndex(int index) {
        this.index = index;
    }

    /**
     * Here, we don't use formatting or anything, we just use a regular message
     * raw.
     * 
     * This will automatically show extended information, as this can be passed
     * to a pastebin service.
     * 
     * @return
     */
    public String getRawMessage() {
        final StringBuilder msg = new StringBuilder();
        msg.append( ( a.getType().doesCreateBlock() || a.getType().getName().equals( "item-insert" ) || a.getType()
                .getName().equals( "sign-change" ) ) ? "+" : "-" );
        msg.append( " #" + a.getId() );
        msg.append( " " + a.getPlayerName() );
        msg.append( " " + a.getType().getName() );
        msg.append( " " + a.getBlockId() + ":" + a.getBlockSubId() );
        if( a.getType().getHandler() != null ) {
            if( !a.getNiceName().isEmpty() )
                msg.append( " (" + a.getNiceName() + ")" );
        } else {
            // We should really improve this, but this saves me from having to
            // make
            // a custom handler.
            if( a.getType().getName().equals( "lava-bucket" ) ) {
                msg.append( " (lava)" );
            } else if( a.getType().getName().equals( "water-bucket" ) ) {
                msg.append( " (water)" );
            }
        }
        if( a.getAggregateCount() > 1 ) {
            msg.append( " x" + a.getAggregateCount() );
        }
        msg.append( " " + a.getDisplayDate() );
        msg.append( " " + a.getDisplayTime().toLowerCase() );
        msg.append( " - " + a.getWorldName() + " @ " + a.getX() + " " + a.getY() + " " + a.getZ() );
        return msg.toString();
    }

    /**
	 * 
	 */
    public String[] getMessage() {

        String[] msg = new String[1];
        if( showExtended ) {
            msg = new String[2];
        }

        final ChatColor highlight = ChatColor.DARK_AQUA;

        String line1 = "";

        // +/-
        line1 += getPosNegPrefix();

        // Result index for teleporting
        if( index > 0 ) {
            line1 += ChatColor.GRAY + " [" + index + "] ";
        }

        // Who
        line1 += highlight + a.getPlayerName();

        // Description of event
        line1 += " " + ChatColor.WHITE + a.getType().getNiceDescription();
        if( a.getType().getHandler() != null ) {
            if( !a.getNiceName().isEmpty() )
                line1 += " " + highlight + a.getNiceName();
        } else {
            // We should really improve this, but this saves me from having to
            // make
            // a custom handler.
            if( a.getType().getName().equals( "lava-bucket" ) ) {
                line1 += " " + highlight + "lava";
            } else if( a.getType().getName().equals( "water-bucket" ) ) {
                line1 += " " + highlight + "water";
            }
        }

        if( showExtended ) {
            line1 += " " + a.getBlockId() + ":" + a.getBlockSubId();
        }

        // Aggregate count
        if( a.getAggregateCount() > 1 ) {
            line1 += ChatColor.GREEN + " x" + a.getAggregateCount();
        }

        // Time since
        if( !a.getTimeSince().isEmpty() ) {
            line1 += ChatColor.WHITE + " " + a.getTimeSince();
        }

        // Action type reminder
        line1 += " " + ChatColor.GRAY + "(a:" + a.getType().getShortName() + ")";

        // Line 2
        String line2 = ChatColor.GRAY + " --";

        line2 += ChatColor.GRAY + " " + a.getId() + " - ";

        // Date & Time
        if( showExtended ) {
            line2 += ChatColor.GRAY + a.getDisplayDate();
            line2 += " " + ChatColor.GRAY + a.getDisplayTime().toLowerCase();

            line2 += " - " + a.getWorldName() + " @ " + a.getX() + " " + a.getY() + " " + a.getZ() + " ";
        }

        msg[0] = line1;
        if( showExtended ) {
            msg[1] = line2;
        }

        return msg;

    }

    /**
     * 
     * @return
     */
    protected String getPosNegPrefix() {

        if( a.getType().doesCreateBlock() || a.getType().getName().equals( "item-insert" )
                || a.getType().getName().equals( "sign-change" ) ) {
            return ChatColor.GREEN + " + " + ChatColor.WHITE;
        } else {
            return ChatColor.RED + " - " + ChatColor.WHITE;
        }
    }
}