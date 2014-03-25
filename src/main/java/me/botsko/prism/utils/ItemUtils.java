package me.botsko.prism.utils;

public class ItemUtils extends me.botsko.elixr.ItemUtils {

    /**
     * 
     * @param item_id
     * @param sub_id
     * @return
     */
    public static boolean isAcceptableWand(int item_id, byte sub_id) {

        // Water/lava
        if( item_id >= 8 && item_id <= 11 ) { return false; }
        // Fire
        if( item_id == 51 || item_id == 259 ) { return false; }
        // Portal
        if( item_id == 90 || item_id == 119 ) { return false; }
        // Monster
        if( item_id == 383 ) { return false; }
        return true;
    }
}