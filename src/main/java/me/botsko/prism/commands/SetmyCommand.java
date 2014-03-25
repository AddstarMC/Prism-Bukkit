package me.botsko.prism.commands;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.settings.Settings;
import me.botsko.prism.utils.ItemUtils;
import me.botsko.prism.wands.Wand;
import org.bukkit.ChatColor;

import java.util.ArrayList;
import java.util.List;

public class SetmyCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     * @return
     */
    public SetmyCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(CallInfo call) {

        String setType = null;
        if( call.getArgs().length >= 2 ) {
            setType = call.getArg( 1 );
        }

        /**
         * Inspector wand
         */
        if( setType != null && !setType.equalsIgnoreCase( "wand" ) ) {
            call.getPlayer().sendMessage( Prism.messenger.playerError( "Invalid arguments. Use /prism ? for help." ) );
            return;
        }

        if( !plugin.getConfig().getBoolean( "prism.wands.allow-user-override" ) ) {
            call.getPlayer().sendMessage(
                    Prism.messenger.playerError( "Sorry, but personalizing the wand is currently not allowed." ) );
        }

        // Check for any wand permissions. @todo There should be some central
        // way to handle this - some way to centralize it at least
        if( !call.getPlayer().hasPermission( "prism.rollback" ) && !call.getPlayer().hasPermission( "prism.restore" )
                && !call.getPlayer().hasPermission( "prism.wand.*" )
                && !call.getPlayer().hasPermission( "prism.wand.inspect" )
                && !call.getPlayer().hasPermission( "prism.wand.profile" )
                && !call.getPlayer().hasPermission( "prism.wand.rollback" )
                && !call.getPlayer().hasPermission( "prism.wand.restore" ) ) {
            call.getPlayer().sendMessage( Prism.messenger.playerError( "You do not have permission for this." ) );
            return;
        }

        // Disable any current wand
        if( Prism.playersWithActiveTools.containsKey( call.getPlayer().getName() ) ) {
            final Wand oldwand = Prism.playersWithActiveTools.get( call.getPlayer().getName() );
            oldwand.disable( call.getPlayer() );
            Prism.playersWithActiveTools.remove( call.getPlayer().getName() );
            call.getPlayer().sendMessage(
                    Prism.messenger.playerHeaderMsg( "Current wand " + ChatColor.RED + "disabled" + ChatColor.WHITE
                            + "." ) );
        }

        String setSubType = null;
        if( call.getArgs().length >= 3 ) {
            setSubType = call.getArg( 2 );
        }

        /**
         * Set your custom wand mode to "hand", "item", or "block"
         */
        if( setSubType != null && setSubType.equalsIgnoreCase( "mode" ) ) {

            String setWandMode = null;
            if( call.getArgs().length >= 4 ) {
                setWandMode = call.getArg( 3 );
            }
            if( setWandMode != null
                    && ( setWandMode.equals( "hand" ) || setWandMode.equals( "item" ) || setWandMode.equals( "block" ) ) ) {
                Settings.saveSetting( "wand.mode", setWandMode, call.getPlayer() );
                // Delete the item so we don't confuse people.
                Settings.deleteSetting( "wand.item", call.getPlayer() );
                call.getPlayer().sendMessage(
                        Prism.messenger.playerHeaderMsg( "Changed your personal wand to " + ChatColor.GREEN
                                + setWandMode + ChatColor.WHITE + " mode." ) );
                return;
            }
            call.getPlayer().sendMessage( Prism.messenger.playerError( "Invalid arguments. Use /prism ? for help." ) );
            return;
        }

        /**
         * Set your custom wand item for either "item" or "block" modes
         */
        if( !setSubType.equalsIgnoreCase( "item" ) ) { return; }
        String setWandItem = null;
        if( call.getArgs().length >= 4 ) {
            setWandItem = call.getArg( 3 );
        }
        if( setWandItem != null ) {

            // If non-numeric, check for name
            if( !TypeUtils.isNumeric( setWandItem ) ) {
                final ArrayList<int[]> itemIds = Prism.getItems().getIdsByAlias( setWandItem );
                if( itemIds.size() > 0 ) {
                    final int[] ids = itemIds.get( 0 );
                    setWandItem = ids[0] + ":" + ids[1];
                } else {
                    call.getPlayer().sendMessage( Prism.messenger.playerError( "There's no item matching that name." ) );
                    return;
                }
            }

            // Standardize
            if( !setWandItem.contains( ":" ) ) {
                setWandItem += ":0";
            }

            int item_id = -1;
            byte item_subid = 0;

            final String[] itemIds = setWandItem.split( ":" );
            if( itemIds.length == 2 ) {
                item_id = Integer.parseInt( itemIds[0] );
                item_subid = Byte.parseByte( itemIds[1] );
            }
            if( item_id > -1 ) {
                final String item_name = Prism.getItems().getAlias( item_id, item_subid );
                if( item_name != null ) {

                    if( !ItemUtils.isAcceptableWand( item_id, item_subid ) ) {
                        call.getPlayer().sendMessage(
                                Prism.messenger
                                        .playerError( "Sorry, but you may not use " + item_name + " for a wand." ) );
                        return;
                    }

                    Settings.saveSetting( "wand.item", setWandItem, call.getPlayer() );
                    call.getPlayer().sendMessage(
                            Prism.messenger.playerHeaderMsg( "Changed your personal wand item to " + ChatColor.GREEN
                                    + item_name + ChatColor.WHITE + "." ) );
                    return;

                }
            }
        }
        call.getPlayer().sendMessage( Prism.messenger.playerError( "Invalid arguments. Use /prism ? for help." ) );
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}