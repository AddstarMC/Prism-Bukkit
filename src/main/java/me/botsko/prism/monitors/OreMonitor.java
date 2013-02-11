package me.botsko.prism.monitors;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.ActionType;

import org.bukkit.ChatColor;
import org.bukkit.GameMode;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.entity.Player;

public class OreMonitor {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	protected Player player;
	
	/**
	 * 
	 */
	protected Block block;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public OreMonitor( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 */
	public void processAlertsFromBlock( final Player player, final Block block ){
		
		if(!plugin.getConfig().getBoolean("prism.alerts.ores.enabled")){
			return;
		}
	
		if( !player.getGameMode().equals(GameMode.CREATIVE) ){
			
			if(block != null && isWatched(block) && !plugin.alertedBlocks.containsKey( block.getLocation() )){
			
				// check if block placed
				boolean wasplaced = false;
				
				// Build params
				QueryParameters params = new QueryParameters();
				params.setWorld( player.getWorld().getName() );
				params.setSpecificBlockLocation(block.getLocation());
				params.addActionType(ActionType.BLOCK_PLACE);
				
				ActionsQuery aq = new ActionsQuery(plugin);
				QueryResult results = aq.lookup( params, player );
				if(!results.getActionResults().isEmpty()){
					wasplaced = true;
				}
				
				if(!wasplaced){
					// identify all ore blocks on same Y axis in x/z direction
					ArrayList<Block> matchingBlocks = new ArrayList<Block>();
					ArrayList<Block> foundores = findNeighborBlocks( block.getType(), block, matchingBlocks );
					if(!foundores.isEmpty()){
						
						// Save the block
						BlockState state = block.getState();
						
						// Set to air to get the light
						block.setType(Material.AIR);
						int light = block.getLightLevel();
						light = (light > 0 ? Math.round(((light) & 0xFF) * 100) / 15 : 0);
						
						// Restore the block
						block.setType( state.getType() );
						
						String msg = getOreColor(block) + player.getName() + " found " + foundores.size() + " " + getOreNiceName(block) + " " + light + "% light";
						
						// Alert staff
						plugin.alertPlayers( null, msg );
						
						// Log to console
						if(plugin.getConfig().getBoolean("prism.alerts.ores.log-to-console")){
							plugin.log( msg );
						}
					}
				}
			}
		}
	}
	
	
	/**
	 * 
	 * @param block
	 * @return
	 */
	protected ChatColor getOreColor( Block block ){
		Material type = block.getType();
		switch(type){
			case DIAMOND_ORE:
				return ChatColor.AQUA;
			case LAPIS_ORE:
				return ChatColor.BLUE;
			case GOLD_ORE:
				return ChatColor.GOLD;
			case IRON_ORE:
				return ChatColor.GRAY;
			case GLOWING_REDSTONE_ORE:
			case REDSTONE_ORE:
				return ChatColor.RED;
			case EMERALD_ORE:
				return ChatColor.GREEN;
			default:
				return ChatColor.WHITE;
		}
	}
	
	
	/**
	 * 
	 * @param block
	 * @return
	 */
	protected String getOreNiceName( Block block ){
		return block.getType().toString().replace("_", " ").toLowerCase().replace("glowing", " ");
	}
	
	
	/**
	 * 
	 * @param block
	 * @return
	 */
	protected boolean isWatched( Block block ){
		
		Material type = block.getType();

		if(type == Material.DIAMOND_ORE && plugin.getConfig().getBoolean("prism.alerts.ores.diamond")){
			return true;
		}
		if(type == Material.GOLD_ORE && plugin.getConfig().getBoolean("prism.alerts.ores.gold")){
			return true;
		}
		if(type == Material.IRON_ORE && plugin.getConfig().getBoolean("prism.alerts.ores.iron")){
			return true;
		}
		if(type == Material.LAPIS_ORE && plugin.getConfig().getBoolean("prism.alerts.ores.lapis")){
			return true;
		}
		if( (type == Material.GLOWING_REDSTONE_ORE || type == Material.REDSTONE_ORE) && plugin.getConfig().getBoolean("prism.alerts.ores.redstone")){
			return true;
		}
		if(type == Material.COAL_ORE && plugin.getConfig().getBoolean("prism.alerts.ores.coal")){
			return true;
		}
		if(type == Material.EMERALD_ORE && plugin.getConfig().getBoolean("prism.alerts.ores.emerald")){
			return true;
		}
		return false;
	}
	
	
	/**
	 * @todo move this to blockutils
	 * @param currBlock
	 * @param toBeFelled
	 */
    private ArrayList<Block> findNeighborBlocks( Material type, Block currBlock, ArrayList<Block> matchingBlocks ) {

        if(isWatched(currBlock)){
        	
        	matchingBlocks.add(currBlock);
        	java.util.Date date = new java.util.Date();
        	plugin.alertedBlocks.put(currBlock.getLocation(), date.getTime());
        	
        	for(int x = -1; x <= 1; x++){
        		for(int z = -1; z <= 1; z++){
        			for(int y = -1; y <= 1; y++){
	        			Block newblock = currBlock.getRelative(x, y, z);
	        			// ensure it matches the type and wasn't already found
	        			if( newblock.getType() == type && !matchingBlocks.contains(newblock) ){
	        				findNeighborBlocks( type, newblock, matchingBlocks );
	        			}
	        		}
        		}
        	}
        }
        
        return matchingBlocks;
        
    }
}
