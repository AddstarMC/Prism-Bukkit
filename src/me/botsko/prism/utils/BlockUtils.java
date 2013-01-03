package me.botsko.prism.utils;

import java.util.ArrayList;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;

public class BlockUtils {
	
	
	/**
	 * Recursively grabs a list of all blocks directly above Block
	 * that are anticipated to fall.
	 * 
	 * @param block
	 * @return
	 */
	public static ArrayList<Block> findFallingBlocksAboveBlock( Block block ){
		
		ArrayList<Block> falling_blocks = new ArrayList<Block>();
		
		// Get block above
		Block above = block.getRelative(BlockFace.UP);
		if(BlockUtils.isFallingBlock(above)){
			falling_blocks.add(above);
			ArrayList<Block> fallingBlocksAbove = findFallingBlocksAboveBlock( above );
			if(fallingBlocksAbove.size() > 0){
				for(Block _temp : fallingBlocksAbove){
					falling_blocks.add(_temp);
				}
			}
		}
		return falling_blocks;
	}
	
	
	/**
	 * Determine whether or not a block is capable of falling.
	 * 
	 * Seems like there's got to be another way to do this...
	 * @param block
	 * @return
	 */
	public static boolean isFallingBlock( Block block ){
		Material m = block.getType();
		if( m.equals(Material.SAND) || m.equals(Material.GRAVEL) || m.equals(Material.ANVIL) ){
			return true;
		}
		return false;
	}
	
	
	/**
	 * Extinguish all the fire in a radius
	 * @param loc The location you want to extinguish around
	 * @param radius The radius around the location you are extinguish
	 */
	public static void extinguish(Location loc, int radius){
		int x1 = loc.getBlockX();
		int y1 = loc.getBlockY();
		int z1 = loc.getBlockZ();
		World world = loc.getWorld();
		for(int x = x1-radius; x <= x1+radius; x++){
			for(int y = y1-radius; y <= y1+radius; y++){
				for(int z = z1-radius; z <= z1+radius; z++){
					loc = new Location(world, x, y, z);
					if( loc.getBlock().getType() == Material.FIRE ){
						loc.getBlock().setType(Material.AIR);
					}
				}
			}
		}
	}
	
	
	/**
	 * Extinguish all the fire in a radius
	 * @param loc The location you want to extinguish around
	 * @param radius The radius around the location you are extinguish
	 */
	public static void drain(Location loc, int radius){
		int x1 = loc.getBlockX();
		int y1 = loc.getBlockY();
		int z1 = loc.getBlockZ();
		World world = loc.getWorld();
		for(int x = x1-radius; x <= x1+radius; x++){
			for(int y = y1-radius; y <= y1+radius; y++){
				for(int z = z1-radius; z <= z1+radius; z++){
					loc = new Location(world, x, y, z);
					if( loc.getBlock().getType() == Material.LAVA || loc.getBlock().getType() == Material.WATER ){
						loc.getBlock().setType(Material.AIR);
					}
				}
			}
		}
	}
}