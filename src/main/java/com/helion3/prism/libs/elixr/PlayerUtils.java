package com.helion3.prism.libs.elixr;

import org.bukkit.Bukkit;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.Player;
import org.bukkit.potion.PotionEffect;

public class PlayerUtils {
    
    
    /**
     * 
     * @param p
     */
    public static void resetPlayer( Player p ){
        
        // Inventory
        p.closeInventory();
        p.getInventory().clear();
        p.getInventory().setArmorContents(null);
        InventoryUtils.updateInventory(p);
        
        // Only if player alive
        if( !p.isDead() ){
            
            // Health/Food
            p.setHealth( p.getMaxHealth() );
            p.setFoodLevel( 20 );
            
            // Potion Effects
            for (final PotionEffect effect : p.getActivePotionEffects()) {
                p.removePotionEffect(effect.getType());
            }
            
            // On fire
            p.setFireTicks( 0 );
            
        }
        
        // Scoreboard
        p.setScoreboard(Bukkit.getScoreboardManager().getNewScoreboard());
        
        // Turn off flight
        p.setAllowFlight( false );
        p.setFlying( false );

    }

	/**
	 * http://forums.bukkit.org/threads/directions.91550/#post-1265631
	 * @param player
	 * @return
	 */
	public static BlockFace getPlayerFacing(Player player){
	 
        float y = player.getLocation().getYaw();
        if( y < 0 ) y += 360;
        y %= 360;
        int i = (int)((y+8) / 22.5);
       
        if(i == 0) return BlockFace.WEST;
        else if(i == 1) return BlockFace.NORTH_WEST;
        else if(i == 2) return BlockFace.NORTH_WEST;
        else if(i == 3) return BlockFace.NORTH_WEST;
        else if(i == 4) return BlockFace.NORTH;
        else if(i == 5) return BlockFace.NORTH_EAST;
        else if(i == 6) return BlockFace.NORTH_EAST;
        else if(i == 7) return BlockFace.NORTH_EAST;
        else if(i == 8) return BlockFace.EAST;
        else if(i == 9) return BlockFace.SOUTH_EAST;
        else if(i == 10) return BlockFace.SOUTH_EAST;
        else if(i == 11) return BlockFace.SOUTH_EAST;
        else if(i == 12) return BlockFace.SOUTH;
        else if(i == 13) return BlockFace.SOUTH_WEST;
        else if(i == 14) return BlockFace.SOUTH_WEST;
        else if(i == 15) return BlockFace.SOUTH_WEST;
 
        return BlockFace.WEST;
	 
	}
}