package me.botsko.prism.actionlibs;

import net.md_5.bungee.api.ChatColor;
import net.md_5.bungee.api.chat.BaseComponent;
import net.md_5.bungee.api.chat.ClickEvent;
import net.md_5.bungee.api.chat.HoverEvent;
import net.md_5.bungee.api.chat.TextComponent;

import me.botsko.prism.actions.Handler;

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

    public BaseComponent getJSONMessage() {

        final ChatColor highlight = ChatColor.DARK_AQUA;
        // Strikethrough when was rollback
        final boolean strike = a.getWasRollback() == 1;

        TextComponent textComponent = new TextComponent();
        TextComponent extraComponent;

        // Positive/negative prefixing
        if( a.getType().doesCreateBlock() || a.getType().getName().equals( "item-insert" )
                || a.getType().getName().equals( "sign-change" ) ) {
            extraComponent = new TextComponent(" + ");
            extraComponent.setColor(ChatColor.GREEN);
        } else {
            extraComponent = new TextComponent(" - ");
            extraComponent.setColor(ChatColor.RED);
        }
        textComponent.addExtra(extraComponent);

        // Result index for teleporting
        if ( index > 0 ) {
            extraComponent = new TextComponent("[" + index + "] ");
            extraComponent.setClickEvent(
                    new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/pr tp id:" + a.getId()));
            extraComponent.setHoverEvent(
                    new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TextComponent[] {
                            new TextComponent("Click to teleport to " + a.getId() + "\n" + a.getWorldName()
                                    + " @ " + a.getX() + " " + a.getY() + " " + a.getZ()) }));
            textComponent.addExtra(extraComponent);
        }

        // Who
        extraComponent = new TextComponent(a.getPlayerName());
        extraComponent.setColor(highlight);
        extraComponent.setStrikethrough(strike);
        textComponent.addExtra(extraComponent);

        // Description of event
        extraComponent = new TextComponent(" " + a.getType().getNiceDescription());
        extraComponent.setColor(ChatColor.WHITE);
        extraComponent.setStrikethrough(strike);
        if( a.getType().getHandler() != null ) {
            if( !a.getNiceName().isEmpty() ) {
                TextComponent extraExtra = new TextComponent(" " + a.getNiceName());
                extraExtra.setColor(highlight);
                extraExtra.setStrikethrough(strike);
                extraComponent.addExtra(extraExtra);
            }
        } else {
            // We should really improve this, but this saves me from having to
            // make a custom handler.
            String niceBucket = null;
            if( a.getType().getName().equals( "lava-bucket" ) ) {
                niceBucket = " lava";
            } else if( a.getType().getName().equals( "water-bucket" ) ) {
                niceBucket = " water";
            }
            if ( niceBucket != null ) {
                TextComponent extraExtra = new TextComponent(" " + a.getNiceName());
                extraExtra.setColor(highlight);
                extraExtra.setStrikethrough(strike);
                extraComponent.addExtra(extraExtra);
            }
        }
        // Action type reminder
        TextComponent extraExtra = new TextComponent("a:" + a.getType().getShortName());
        extraExtra.setColor(ChatColor.GRAY);
        extraComponent.setHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TextComponent[] { extraExtra }));
        textComponent.addExtra(extraComponent);

        if( showExtended ) {
            extraComponent = new TextComponent(" " + a.getBlockId() + ":" + a.getBlockSubId());
            extraComponent.setColor(ChatColor.GRAY);
            textComponent.addExtra(extraComponent);
        }

        // Aggregate count
        if( a.getAggregateCount() > 1 ) {
            extraComponent = new TextComponent(" x" + a.getAggregateCount());
            extraComponent.setColor(ChatColor.GREEN);
            extraComponent.setStrikethrough(strike);
            textComponent.addExtra(extraComponent);
        }

        // Time since
        if( !a.getTimeSince().isEmpty() ) {
            extraComponent = new TextComponent(" " + a.getTimeSince());
            extraComponent.setColor(ChatColor.WHITE);
            extraComponent.setStrikethrough(strike);
            // Additional date data (line2 of non-JSON message)
            extraExtra = new TextComponent(a.getDisplayDate() + " " + a.getDisplayTime().toLowerCase());
            extraExtra.setColor(ChatColor.GRAY);
            extraComponent.setHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TextComponent[] { extraExtra }));
            textComponent.addExtra(extraComponent);
        }

        return textComponent;
    }
    
    public String[] getMessage() {

        String[] msg = new String[1];
        if( showExtended ) {
            msg = new String[2];
        }

        final ChatColor highlight = ChatColor.DARK_AQUA;

        String line1 = "";

        // Strikethrough when was rollback
        String strike = "";
        if (a.getWasRollback() == 1) {
            strike = "§m";
        }

        // Result index for teleporting
        String indexString = "";
        if (index > 0) {
            indexString = ChatColor.GRAY + " [" + index + "] ";
        }

        // Who
        line1 += highlight + strike + a.getPlayerName();

        // Description of event
        line1 += " " + ChatColor.WHITE + strike + a.getType().getNiceDescription();
        if( a.getType().getHandler() != null ) {
            if( !a.getNiceName().isEmpty() )
                line1 += " " + highlight + strike + a.getNiceName();
        } else {
            // We should really improve this, but this saves me from having to
            // make
            // a custom handler.
            if( a.getType().getName().equals( "lava-bucket" ) ) {
                line1 += " " + highlight + strike + "lava";
            } else if( a.getType().getName().equals( "water-bucket" ) ) {
                line1 += " " + highlight + strike + "water";
            }
        }

        if( showExtended ) {
            line1 += " " + a.getBlockId() + ":" + a.getBlockSubId();
        }

        // Aggregate count
        if( a.getAggregateCount() > 1 ) {
            line1 += ChatColor.GREEN + strike + " x" + a.getAggregateCount();
        }

        // Time since
        if( !a.getTimeSince().isEmpty() ) {
            line1 += ChatColor.WHITE + strike + " " + a.getTimeSince();
        }

        // Action type reminder
        line1 += " " + ChatColor.GRAY + strike + "(a:" + a.getType().getShortName() + ")";

        // Line 2
        String line2 = ChatColor.GRAY + " --";

        line2 += ChatColor.GRAY + " " + a.getId() + " - ";

        // Date & Time
        if( showExtended ) {
            line2 += ChatColor.GRAY + a.getDisplayDate();
            line2 += " " + ChatColor.GRAY + a.getDisplayTime().toLowerCase();

            line2 += " - " + a.getWorldName() + " @ " + a.getX() + " " + a.getY() + " " + a.getZ() + " ";
        }

        msg[0] = getPosNegPrefix() + indexString + line1;

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

    protected BaseComponent getPosNegPrefixComponent() {
        TextComponent component;
        if( a.getType().doesCreateBlock() || a.getType().getName().equals( "item-insert" )
                || a.getType().getName().equals( "sign-change" ) ) {
            component = new TextComponent(" + ");
            component.setColor(ChatColor.GREEN);
        } else {
            component = new TextComponent(" - ");
            component.setColor(ChatColor.RED);
        }
        return component;
    }

}