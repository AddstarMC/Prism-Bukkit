package com.helion3.prism.libs.elixr;

import org.bukkit.Location;
import org.bukkit.World.Environment;

public class WorldUtils {
    
    /**
     * Creates lightning that doesn't strike the ground, only thunder is heard
     * @param loc the location to strike lightning above
     */
    public static void thunder( Location loc ){
        loc.setY(350D);
        if(loc.getWorld().getEnvironment() == Environment.NORMAL){
            loc.getWorld().strikeLightningEffect(loc);
        }
    }
}