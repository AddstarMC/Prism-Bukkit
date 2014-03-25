package me.botsko.prism.utils;

import org.bukkit.Location;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.wands.Wand;

public class WandUtils {

    public static boolean playerUsesWandOnClick(Player player, Location loc) {

        if( Prism.playersWithActiveTools.containsKey( player.getName() ) ) {

            final Wand wand = Prism.playersWithActiveTools.get( player.getName() );

            if( wand == null )
                return false;

            final int item_id = wand.getItemId();
            final byte item_subid = wand.getItemSubId();
            if( player.getItemInHand().getTypeId() == item_id && player.getItemInHand().getDurability() == item_subid ) {
                // Left click is for current location
                wand.playerLeftClick( player, loc );
                return true;
            }
        }

        return false;

    }

}