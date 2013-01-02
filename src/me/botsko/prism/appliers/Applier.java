package me.botsko.prism.appliers;

import org.bukkit.Material;

public class Applier {
	
	
	/**
	 * Returns whether or not the plugin may place a material,
	 * so that we can avoid including dangerous items with an
	 * applier.
	 * 
	 * @param block
	 * @return
	 */
	public boolean mayEverPlace( Material m ){
		if( m.equals(Material.TNT) || m.equals(Material.FIRE) || m.equals(Material.LAVA) ){
			return false;
		}
		return true;
	}

}