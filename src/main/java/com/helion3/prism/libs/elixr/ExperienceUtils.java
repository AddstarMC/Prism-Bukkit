package com.helion3.prism.libs.elixr;

import java.util.Arrays;

import org.bukkit.entity.Player;

/**
* Original implementation by desht
*
* Credit to nisovin for removing the dependency on player.getTotalExperience(), which is
* basically broken (does not account for exp change due to enchanting).
*/
public class ExperienceUtils {
	
    public static final int MAX_LEVEL_SUPPORTED = 150;
    private static final int xpRequiredForNextLevel[] = new int[MAX_LEVEL_SUPPORTED];
    private static final int xpTotalToReachLevel[] = new int[MAX_LEVEL_SUPPORTED];
 
    // Initialise the xp lookup table. Basing this on observations noted in https://bukkit.atlassian.net/browse/BUKKIT-47
    // 7 xp to get to level 1, 17 to level 2, 31 to level 3...
    // At each level, the increment to get to the next level increases alternately by 3 and 4
    static {
            xpTotalToReachLevel[0] = 0;
            int incr = 7;
            for (int i = 1; i < xpTotalToReachLevel.length; i++) {
                    xpRequiredForNextLevel[i - 1] = incr;
                    xpTotalToReachLevel[i] = xpTotalToReachLevel[i - 1] + incr;
                    incr += (i % 2 == 0) ? 4 : 3;
            }
    }
   
    
    /**
     * 
     * @param player
     * @param amt
     */
    public static void changeExp(Player player, int amt) {
            int xp = getCurrentExp(player) + amt;
            if (xp < 0) xp = 0;
           
            int curLvl = player.getLevel();
            int newLvl = getCurrentLevel(xp);
            if (curLvl != newLvl) {
                    player.setLevel(newLvl);
            }
           
            float pct = ((float)(xp - xpTotalToReachLevel[newLvl]) / (float)xpRequiredForNextLevel[newLvl]);
            player.setExp(pct);
    }
   
    
    /**
     * 
     * @param player
     * @return
     */
    public static int getCurrentExp(Player player) {
            int lvl = player.getLevel();
            return xpTotalToReachLevel[lvl] + (int) (xpRequiredForNextLevel[lvl] * player.getExp());
    }
   
    
    /**
     * 
     * @param player
     * @param amt
     * @return
     */
    public static boolean hasExp(Player player, int amt) {
            return getCurrentExp(player) >= amt;
    }
   
    
    /**
     * 
     * @param exp
     * @return
     */
    public static int getCurrentLevel(int exp) {
            if (exp <= 0) return 0;
            int pos = Arrays.binarySearch(xpTotalToReachLevel, exp);
            return pos < 0 ? -pos - 2 : pos;
    }
}