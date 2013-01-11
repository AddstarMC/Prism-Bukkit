package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.EntityUtils;

public class Rollback extends Preview {
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Rollback( Prism plugin, Player player, PrismProcessType processType, List<Action> results, QueryParameters parameters, long processStartTime ){
		super(plugin, player, processType, results, parameters, processStartTime);
	}
	
	
	/**
	 * Set preview move and then do a rollback
	 * @return
	 */
	public void preview(){
		is_preview = true;
		apply();
	}
	
	
	/**
	 * 
	 */
	public void apply(){
		
		// Remove any fire at this location
		if(plugin.getConfig().getBoolean("prism.appliers.remove-fire-on-burn-rollback") && parameters.getActionTypes().contains(ActionType.BLOCK_BURN)){
			ArrayList<BlockStateChange> blockStateChanges = BlockUtils.extinguish(player.getLocation(),parameters.getRadius());
			if( blockStateChanges != null && !blockStateChanges.isEmpty() ){
				player.sendMessage( plugin.playerHeaderMsg("Extinguishing fire!" + ChatColor.GRAY + " Like a boss.") );
			}
		}
		
		// Remove item drops in this radius
		if(plugin.getConfig().getBoolean("prism.appliers.remove-drops-on-explode-rollback") && (parameters.getActionTypes().contains(ActionType.TNT_EXPLODE) || parameters.getActionTypes().contains(ActionType.CREEPER_EXPLODE)) ){
			int removed = EntityUtils.removeNearbyItemDrops(player, parameters.getRadius());
			if(removed > 0){
				player.sendMessage( plugin.playerHeaderMsg("Removed " + removed + " drops in affected area." + ChatColor.GRAY + " Like a boss.") );
			}
		}
		
//		// Remove any liquid at this location
//		if(plugin.getConfig().getBoolean("prism.appliers.remove-liquid-on-flow-rollback") && ( parameters.getActionTypes().contains(ActionType.WATER_FLOW) || parameters.getActionTypes().contains(ActionType.LAVA_FLOW)) ){
//			int fires_ext = BlockUtils.drain(player.getLocation(),parameters.getRadius());
//			if(fires_ext > 0){
//				responses.add( plugin.playerHeaderMsg("Draining liquid first!" + ChatColor.GRAY + " Like a boss.") );
//			}
//		}
		
		// @todo can't really work here. doesn't return a proper result, etc
		// Remove any lava blocks when doing a lava bucket rollback
//		if(parameters.getActionTypes().contains(ActionType.LAVA_BUCKET) || parameters.getActionTypes().contains(ActionType.LAVA_BREAK)){
//			BlockUtils.drainlava(parameters.getPlayerLocation(), parameters.getRadius());
//		}
//		if(parameters.getActionTypes().contains(ActionType.WATER_BUCKET) || parameters.getActionTypes().contains(ActionType.WATER_BREAK)){
//			BlockUtils.drainwater(parameters.getPlayerLocation(), parameters.getRadius());
//		}
	
			
		// Give the results to the changequeue
		super.apply();
		
	}
}