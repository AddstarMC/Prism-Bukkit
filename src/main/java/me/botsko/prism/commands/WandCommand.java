package me.botsko.prism.commands;

import me.botsko.elixr.InventoryUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.settings.Settings;
import me.botsko.prism.utils.ItemUtils;
import me.botsko.prism.wands.*;
import org.bukkit.ChatColor;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;

import java.util.List;

public class WandCommand implements SubHandler {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     * @return
     */
    public WandCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(CallInfo call) {
        String type = "i";
        final boolean isInspect = call.getArg( 0 ).equalsIgnoreCase( "inspect" )
                || call.getArg( 0 ).equalsIgnoreCase( "i" );
        if( !isInspect ) {
            if( call.getArgs().length < 2 ) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerError( "You need to specify a wand type. Use '/prism ?' for help." ) );
                return;
            }
            type = call.getArg( 1 );
        }

        Wand oldwand = null;
        if( Prism.playersWithActiveTools.containsKey( call.getPlayer().getName() ) ) {
            // Pull the wand in use
            oldwand = Prism.playersWithActiveTools.get( call.getPlayer().getName() );
        }

        // Always remove the old one
        Prism.playersWithActiveTools.remove( call.getPlayer().getName() );

        // Determine default mode
        String mode = plugin.getConfig().getString( "prism.wands.default-mode" );

        // Check if the player has a personal override
        if( plugin.getConfig().getBoolean( "prism.wands.allow-user-override" ) ) {
            final String personalMode = Settings.getSetting( "wand.mode", call.getPlayer() );
            if( personalMode != null ) {
                mode = personalMode;
            }
        }

        // Determine which item we're using.
        int item_id = 0;
        byte item_subid = -1;
        String toolKey = null;
        if( mode.equals( "item" ) ) {
            toolKey = plugin.getConfig().getString( "prism.wands.default-item-mode-id" );
        } else if( mode.equals( "block" ) ) {
            toolKey = plugin.getConfig().getString( "prism.wands.default-block-mode-id" );
        }

        // Check if the player has a personal override
        if( plugin.getConfig().getBoolean( "prism.wands.allow-user-override" ) ) {
            final String personalToolKey = Settings.getSetting( "wand.item", call.getPlayer() );
            if( personalToolKey != null ) {
                toolKey = personalToolKey;
            }
        }

        if( toolKey != null ) {
            if( !toolKey.contains( ":" ) ) {
                toolKey += ":0";
            }
            final String[] toolKeys = toolKey.split( ":" );
            item_id = Integer.parseInt( toolKeys[0] );
            item_subid = Byte.parseByte( toolKeys[1] );
        }

        String wandOn = "";
        String item_name = "";
        String parameters = "";
        if( item_id != 0 ) {
            item_name = Prism.getItems().getAlias( item_id, item_subid );
            wandOn += " on a " + item_name;
        }

        for ( int i = ( isInspect ? 1 : 2 ); i < call.getArgs().length; i++ ) {
            if( parameters.isEmpty() ) {
                parameters += " using:" + ChatColor.GRAY;
            }
            parameters += " " + call.getArg( i );
        }

        if( !ItemUtils.isAcceptableWand( item_id, item_subid ) ) {
            call.getPlayer().sendMessage(
                    Prism.messenger.playerError( "Sorry, but you may not use " + item_name + " for a wand." ) );
            return;
        }

        boolean enabled = false;
        Wand wand = null;

        /**
         * Inspector wand
         */
        if( type.equalsIgnoreCase( "i" ) || type.equalsIgnoreCase( "inspect" ) ) {
            if( !call.getPlayer().hasPermission( "prism.lookup" )
                    && !call.getPlayer().hasPermission( "prism.wand.inspect" ) ) {
                call.getPlayer().sendMessage( Prism.messenger.playerError( "You do not have permission for this." ) );
                return;
            }
            if( oldwand != null && oldwand instanceof InspectorWand ) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerHeaderMsg( "Inspection wand " + ChatColor.RED + "disabled"
                                + ChatColor.WHITE + "." ) );
            } else {
                wand = new InspectorWand( plugin );
                call.getPlayer().sendMessage(
                        Prism.messenger.playerHeaderMsg( "Inspection wand " + ChatColor.GREEN + "enabled"
                                + ChatColor.WHITE + wandOn + parameters + "." ) );
                enabled = true;
            }
        }

        /**
         * Profile wand
         */
        else if( type.equalsIgnoreCase( "p" ) || type.equalsIgnoreCase( "profile" ) ) {
            if( !call.getPlayer().hasPermission( "prism.lookup" )
                    && !call.getPlayer().hasPermission( "prism.wand.profile" ) ) {
                call.getPlayer().sendMessage( Prism.messenger.playerError( "You do not have permission for this." ) );
                return;
            }
            if( oldwand != null && oldwand instanceof ProfileWand ) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerHeaderMsg( "Profile wand " + ChatColor.RED + "disabled" + ChatColor.WHITE
                                + "." ) );
            } else {
                wand = new ProfileWand();
                call.getPlayer().sendMessage(
                        Prism.messenger.playerHeaderMsg( "Profile wand " + ChatColor.GREEN + "enabled"
                                + ChatColor.WHITE + wandOn + "." ) );
                enabled = true;
            }
        }

        /**
         * Rollback wand
         */
        else if( type.equalsIgnoreCase( "rollback" ) || type.equalsIgnoreCase( "rb" ) ) {
            if( !call.getPlayer().hasPermission( "prism.rollback" )
                    && !call.getPlayer().hasPermission( "prism.wand.rollback" ) ) {
                call.getPlayer().sendMessage( Prism.messenger.playerError( "You do not have permission for this." ) );
                return;
            }
            if( oldwand != null && oldwand instanceof RollbackWand ) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerHeaderMsg( "Rollback wand " + ChatColor.RED + "disabled"
                                + ChatColor.WHITE + "." ) );
            } else {
                wand = new RollbackWand( plugin );
                call.getPlayer().sendMessage(
                        Prism.messenger.playerHeaderMsg( "Rollback wand " + ChatColor.GREEN + "enabled"
                                + ChatColor.WHITE + wandOn + parameters + "." ) );
                enabled = true;
            }
        }

        /**
         * Restore wand
         */
        else if( type.equalsIgnoreCase( "restore" ) || type.equalsIgnoreCase( "rs" ) ) {
            if( !call.getPlayer().hasPermission( "prism.restore" )
                    && !call.getPlayer().hasPermission( "prism.wand.restore" ) ) {
                call.getPlayer().sendMessage( Prism.messenger.playerError( "You do not have permission for this." ) );
                return;
            }
            // If disabling this one
            if( oldwand != null && oldwand instanceof RestoreWand ) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerHeaderMsg( "Restore wand " + ChatColor.RED + "disabled" + ChatColor.WHITE
                                + "." ) );
            } else {
                wand = new RestoreWand( plugin );
                call.getPlayer().sendMessage(
                        Prism.messenger.playerHeaderMsg( "Restore wand " + ChatColor.GREEN + "enabled"
                                + ChatColor.WHITE + wandOn + parameters + "." ) );
                enabled = true;
            }
        }

        /**
         * Off
         */
        else if( type.equalsIgnoreCase( "off" ) ) {
            call.getPlayer().sendMessage(
                    Prism.messenger.playerHeaderMsg( "Current wand " + ChatColor.RED + "disabled" + ChatColor.WHITE
                            + "." ) );
        }

        // Not a valid wand
        else {
            call.getPlayer().sendMessage( Prism.messenger.playerError( "Invalid wand type. Use /prism ? for help." ) );
            return;
        }

        final PlayerInventory inv = call.getPlayer().getInventory();
        if( enabled ) {

            wand.setWandMode( mode );
            wand.setItemId( item_id );
            wand.setItemSubId( item_subid );

            Prism.debug( "Wand activated for player - mode: " + mode + " Item:" + item_id + ":" + item_subid );

            // Move any existing item to the hand, otherwise give it to them
            if( plugin.getConfig().getBoolean( "prism.wands.auto-equip" ) ) {
                if( !InventoryUtils.moveItemToHand( inv, item_id, item_subid ) ) {
                    // Store the item they're holding, if any
                    wand.setOriginallyHeldItem( inv.getItemInHand() );
                    // They don't have the item, so we need to give them an item
                    if( InventoryUtils.handItemToPlayer( inv, new ItemStack( item_id, 1, item_subid ) ) ) {
                        wand.setItemWasGiven( true );
                    } else {
                        call.getPlayer().sendMessage(
                                Prism.messenger.playerError( "Can't fit the wand item into your inventory." ) );
                    }
                }
                call.getPlayer().updateInventory();
            }

            // Let's build the QueryParameters for it if it's a Query wand.
            if( wand instanceof QueryWandBase ) {
                if( !( (QueryWandBase) wand ).setParameters( call.getPlayer(), call.getArgs(), ( isInspect ? 1 : 2 ) ) ) { // This
                                                                                                                           // returns
                                                                                                                           // if
                                                                                                                           // it
                                                                                                                           // was
                                                                                                                           // successful
                    call.getPlayer().sendMessage( Prism.messenger.playerError( "Notice: Only some parameters used.." ) );
                }
            }

            // Store
            Prism.playersWithActiveTools.put( call.getPlayer().getName(), wand );
        } else {
            if( oldwand != null ) {
                oldwand.disable( call.getPlayer() );
            }
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}