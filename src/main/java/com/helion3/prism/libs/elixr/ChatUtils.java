package com.helion3.prism.libs.elixr;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.entity.Player;

public class ChatUtils {
    
    /**
     * 
     * @param player
     * @param msg
     */
    public static void notifyNearby( Location loc, int radius, String msg ){
        for ( final Player p : Bukkit.getServer().getOnlinePlayers() ) {
            if( loc.getWorld().equals( p.getWorld() ) ) {
                if( loc.distance( p.getLocation() ) <= radius ) {
                    p.sendMessage( msg );
                }
            }
        }
    }
}