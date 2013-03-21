package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.EntityUtils;

public class Rollback extends Preview {

    protected Prism prism;
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Rollback( Prism plugin, CommandSender sender, List<Handler> results, QueryParameters parameters, ApplierCallback callback ){
		super(plugin, sender, results, parameters, callback);
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
		if(plugin.getConfig().getBoolean("prism.appliers.remove-fire-on-burn-rollback") && parameters.getActionTypes().containsKey("block-burn")){
			if( !parameters.hasFlag(Flag.NO_EXT) ){
				ArrayList<BlockStateChange> blockStateChanges = BlockUtils.extinguish(player.getLocation(),parameters.getRadius());
				if( blockStateChanges != null && !blockStateChanges.isEmpty() ){
					player.sendMessage( Prism.messenger.playerHeaderMsg("Extinguishing fire!" + (prism.getConfig().getBoolean("prism.messages.assurances", true) ? ChatColor.GRAY + " Like a boss." : null)) );
				}
			}
		}
		
		// Remove item drops in this radius
		if(plugin.getConfig().getBoolean("prism.appliers.remove-drops-on-explode-rollback") && (parameters.getActionTypes().containsKey("tnt-explode") || parameters.getActionTypes().containsKey("creeper-explode")) ){
			if( !parameters.hasFlag(Flag.NO_ITEMCLEAR) ){
				int removed = EntityUtils.removeNearbyItemDrops(player, parameters.getRadius());
				if(removed > 0){
					player.sendMessage( Prism.messenger.playerHeaderMsg("Removed " + removed + " drops in affected area." + (prism.getConfig().getBoolean("prism.messages.assurances", true) ? ChatColor.GRAY + " Like a boss." : null)) );
				}
			}
		}
		
		// Remove any liquid at this location
		ArrayList<BlockStateChange> drained = null;
		if( parameters.hasFlag(Flag.DRAIN) ){
			drained = BlockUtils.drain(player.getLocation(),parameters.getRadius());
		}
		if( parameters.hasFlag(Flag.DRAIN_LAVA) ){
			drained = BlockUtils.drainlava(player.getLocation(),parameters.getRadius());
		}
		if( parameters.hasFlag(Flag.DRAIN_WATER) ){
			drained = BlockUtils.drainwater(player.getLocation(),parameters.getRadius());
		}
		if(drained != null && drained.size() > 0){
			player.sendMessage( Prism.messenger.playerHeaderMsg("Draining liquid!" + (prism.getConfig().getBoolean("prism.messages.assurances", true) ? ChatColor.GRAY + " Like a boss." : null)) );
		}
			
		// Give the results to the changequeue
		super.apply();
		
	}
}