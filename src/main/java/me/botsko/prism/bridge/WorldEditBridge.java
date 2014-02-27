package me.botsko.prism.bridge;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;

import org.bukkit.entity.Player;
import org.bukkit.util.Vector;

import com.sk89q.worldedit.IncompleteRegionException;
import com.sk89q.worldedit.LocalPlayer;
import com.sk89q.worldedit.LocalWorld;
import com.sk89q.worldedit.bukkit.BukkitPlayer;
import com.sk89q.worldedit.bukkit.selections.Selection;
import com.sk89q.worldedit.regions.Region;

public class WorldEditBridge {

	
	/**
	 * 
	 * @param plugin
	 * @param player
	 * @param parameters
	 * @return
	 */
	public static boolean getSelectedArea(Prism plugin, Player player, QueryParameters parameters ){
		// Get selected area
		Region region;
		try {
			LocalPlayer lp = new BukkitPlayer(Prism.plugin_worldEdit, Prism.plugin_worldEdit.getWorldEdit().getServer(), player);
			LocalWorld lw = lp.getWorld();
			region = Prism.plugin_worldEdit.getWorldEdit().getSession(lp).getSelection(lw);
		} catch (IncompleteRegionException e) {
			return false;
		}
		
		//Set WorldEdit locations
		Vector minLoc = new Vector(region.getMinimumPoint().getX(), region.getMinimumPoint().getY(), region.getMinimumPoint().getZ());
		Vector maxLoc = new Vector(region.getMaximumPoint().getX(), region.getMaximumPoint().getY(), region.getMaximumPoint().getZ());
		
		// Check selection against max radius
		Selection sel = Prism.plugin_worldEdit.getSelection(player);
		double lRadius = Math.ceil(sel.getLength() / 2);
		double wRadius = Math.ceil(sel.getWidth() / 2);
		double hRadius = Math.ceil(sel.getHeight() / 2);
		
		String procType = "applier";
		if( parameters.getProcessType().equals(PrismProcessType.LOOKUP) ){
			procType = "lookup";
		}
		
		int maxRadius = plugin.getConfig().getInt("prism.queries.max-"+procType+"-radius");
		if (maxRadius != 0 && (lRadius > maxRadius || wRadius > maxRadius || hRadius > maxRadius) && !player.hasPermission("prism.override-max-"+procType+"-radius") ){
			return false;
		} else {
			
			parameters.setWorld(region.getWorld().getName());
			parameters.setMinLocation(minLoc);
			parameters.setMaxLocation(maxLoc);
			
		}
		return true;
	}
}
