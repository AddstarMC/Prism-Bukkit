package com.helion3.prism.libs.elixr;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Hanging;
import org.bukkit.material.Bed;

import java.util.ArrayList;
import java.util.Collection;

public class BlockUtils {

	/**
	 * Determines if the material of an existing block at a location is
	 * something that's commonly acceptable to replace.
	 * 
	 * @param m the material of the block
	 * @return if the material is acceptable to replace
	 */
	public static boolean isAcceptableForBlockPlace( Material m ){
		switch(m){
			case AIR:
			case FIRE:
			case GRAVEL:
			case LAVA:
            case LONG_GRASS:
			case SAND:
            case SNOW:
            case SNOW_BLOCK:
            case STATIONARY_LAVA:
			case STATIONARY_WATER:
            case WATER:
				return true;
			default:
				return false;
		}
	}

	/**
	 * Recursively grabs a list of all blocks directly above Block
	 * that are anticipated to fall.
	 * 
	 * @param block the block to fetch blocks above
	 * @return the list of blocks directly above the block
	 */
	public static ArrayList<Block> findFallingBlocksAboveBlock( final Block block ){
		ArrayList<Block> falling_blocks = new ArrayList<Block>();
		
		// Get block above
		Block above = block.getRelative(BlockFace.UP);
		if( above.getType().hasGravity() ){
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
	 * Searches for detachable blocks on the four acceptable sides of a block.
	 * 
	 * @param block the block to check the sides of
	 * @return the list of detachable block on the sides of the block
	 */
	public static ArrayList<Block> findSideFaceAttachedBlocks( final Block block ){
		
		ArrayList<Block> detaching_blocks = new ArrayList<Block>();
		
		// Check each of the four sides
		Block blockToCheck = block.getRelative(BlockFace.EAST);
		if(BlockUtils.isSideFaceDetachableMaterial(blockToCheck.getType())){
			detaching_blocks.add(blockToCheck);
		}
		blockToCheck = block.getRelative(BlockFace.WEST);
		if(BlockUtils.isSideFaceDetachableMaterial(blockToCheck.getType())){
			detaching_blocks.add(blockToCheck);
		}
		blockToCheck = block.getRelative(BlockFace.NORTH);
		if(BlockUtils.isSideFaceDetachableMaterial(blockToCheck.getType())){
			detaching_blocks.add(blockToCheck);
		}
		blockToCheck = block.getRelative(BlockFace.SOUTH);
		if(BlockUtils.isSideFaceDetachableMaterial(blockToCheck.getType())){
			detaching_blocks.add(blockToCheck);
		}
		
		return detaching_blocks;
		
	}
	
	/**
	 * Searches around a block for the first block of the given material
     *
	 * @param block the block to search around
	 * @param m the material of the surrounding block to look for
	 * @return the first surrounding block of the given material found
	 */
	public static Block findFirstSurroundingBlockOfType( Block block, Material m){
		Block blockToCheck = block.getRelative(BlockFace.EAST);
		if( blockToCheck.getType().equals(m) ){
			return blockToCheck;
		}
		blockToCheck = block.getRelative(BlockFace.WEST);
		if( blockToCheck.getType().equals(m) ){
			return blockToCheck;
		}
		blockToCheck = block.getRelative(BlockFace.NORTH);
		if( blockToCheck.getType().equals(m) ){
			return blockToCheck;
		}
		blockToCheck = block.getRelative(BlockFace.SOUTH);
		if( blockToCheck.getType().equals(m) ){
			return blockToCheck;
		}
		return null;
	}
	
	/**
	 * Determine whether or not a block using the given material is going to detach
	 * from the side of a block.
	 *
	 * @param m the material to check for detaching
	 * @return whether a block with a given material will detach from the side of a block
	 */
	public static boolean isSideFaceDetachableMaterial( Material m ){
        switch (m){
            case BANNER:
            case COCOA:
            case WALL_SIGN:
            case LADDER:
            case LEVER:
            case IRON_TRAPDOOR:
            case PORTAL:
            case PISTON_BASE: // Fake entry, the base always breaks if the extension is lost
            case PISTON_EXTENSION:
            case PISTON_MOVING_PIECE:
            case PISTON_STICKY_BASE:
            case REDSTONE_TORCH_OFF:
            case REDSTONE_TORCH_ON:
            case STANDING_BANNER:
            case STONE_BUTTON:
            case TRAP_DOOR:
            case TORCH:
            case TRIPWIRE_HOOK:
            case WALL_BANNER:
            case WOOD_BUTTON:
            case VINE:
                return true;
            default:
                return false;
        }
	}
	
	/**
	 * Searches for detachable blocks on the four acceptable sides of a block.
	 * 
	 * @param block
	 * @return
	 */
	public static ArrayList<Block> findTopFaceAttachedBlocks( final Block block ){
		ArrayList<Block> detaching_blocks = new ArrayList<Block>();
		
		// Find any block on top of this that will detach
		Block blockToCheck = block.getRelative(BlockFace.UP);
		if(BlockUtils.isTopFaceDetachableMaterial(blockToCheck.getType())){
			detaching_blocks.add(blockToCheck);
			if( blockToCheck.getType().equals(Material.CACTUS) || blockToCheck.getType().equals(Material.SUGAR_CANE_BLOCK) ){
				// For cactus and sugar cane, we can even have blocks above
				ArrayList<Block> additionalBlocks = findTopFaceAttachedBlocks(blockToCheck);
				if(!additionalBlocks.isEmpty()){
					for(Block _temp : additionalBlocks){
						detaching_blocks.add(_temp);
					}
				}
			}
		}
		
		return detaching_blocks;
		
	}
	
	
	/**
	 * Determine whether or not a block is going to detach
	 * from the top of a block.
     *
	 * @param m the material to check for detaching
	 * @return whether a block with a given material will detach from the top of a block
	 */
	public static boolean isTopFaceDetachableMaterial( Material m ){
		switch(m){
            case ACTIVATOR_RAIL:
            case BANNER:
			case BROWN_MUSHROOM:
			case CACTUS:
			case CARROT:
            case CROPS:
			case DEAD_BUSH:
			case DETECTOR_RAIL:
            case DIODE:
            case DIODE_BLOCK_OFF:
            case DIODE_BLOCK_ON:
			case DOUBLE_PLANT:
            case FLOWER_POT:
			case GOLD_PLATE:
			case IRON_DOOR:
			case IRON_DOOR_BLOCK:
			case IRON_PLATE:
			case LEVER:
			case LONG_GRASS:
			case MELON_STEM:
			case NETHER_WARTS:
			case PORTAL:
            case POTATO:
			case POWERED_RAIL:
			case PUMPKIN_STEM:
			case RAILS:
			case RED_MUSHROOM:
			case RED_ROSE:
			case REDSTONE:
			case REDSTONE_COMPARATOR_OFF:
			case REDSTONE_COMPARATOR_ON:
			case REDSTONE_TORCH_OFF:
			case REDSTONE_TORCH_ON:
			case REDSTONE_WIRE:
			case SAPLING:
			case SIGN:
			case SIGN_POST:
			case SKULL:
			case SNOW:
            case STANDING_BANNER:
			case STONE_PLATE:
            case SUGAR_CANE_BLOCK:
			case TORCH:
			case TRIPWIRE:
            case WALL_BANNER:
			case WATER_LILY:
			case WHEAT:
			case WOOD_DOOR:
			case WOOD_PLATE:
			case WOODEN_DOOR:
			case YELLOW_FLOWER:
				return true;
			default:
				return false;
		}
	}
	
	
	/**
	 * Determine whether or not a block location is filled
	 * by a material that means an attachable material
	 * is now detached.
	 * 
	 * @param m
	 * @return
	 */
	public static boolean materialMeansBlockDetachment(Material m){
		switch(m){
			case AIR:
			case FIRE:
			case WATER:
			case STATIONARY_WATER:
			case LAVA:
			case STATIONARY_LAVA:
				return true;
			default:
				return false;
		}
	}


	/**
	 * Searches for hanging entities that are inside the given location
	 *
	 * @param loc
	 * @return
	 */
	public static ArrayList<Hanging> findHangingEntities( final Location loc ) {

		ArrayList<Hanging> entities = new ArrayList<>();

		loc.add(0.5, 0.5, 0.5);
		Collection<Entity> foundEntities = loc.getWorld().getNearbyEntities(loc, 0.5, 0.5, 0.5);
		for (Entity e : foundEntities) {
			// Some modded servers seems to list entities in the chunk
			// that exists in other worlds. No idea why but we can at
			// least check for it.
			// https://snowy-evening.com/botsko/prism/318/
			if( !loc.getWorld().equals( e.getWorld() ) ) continue;
			// Only check hanging entities
			if( isHangingEntity(e) ) entities.add( (Hanging) e );
		}

		return entities;

	}
	
	
	/**
	 * Searches for hanging entities that are attached to the given block
	 * 
	 * @param block
	 * @return
	 */
	public static ArrayList<Hanging> findAttachedHangingEntities( final Block block ){
		
		ArrayList<Hanging> entities = new ArrayList<>();

		Collection<Entity> foundEntities = block.getWorld().getNearbyEntities(block.getLocation(), 1.5, 1.5, 1.5);
		for (Entity e : foundEntities) {
			// Some modded servers seems to list entities in the chunk
			// that exists in other worlds. No idea why but we can at
			// least check for it.
			// https://snowy-evening.com/botsko/prism/318/
			if( !block.getWorld().equals( e.getWorld() ) ) continue;
			// Only check hanging entities
			if( !isHangingEntity(e) ) continue;

			final Hanging hangingEntity = (Hanging) e;
			final BlockFace attachedFace = hangingEntity.getAttachedFace();

			// Only get hanging entities actually attached to this block
			// BUG: Not accurate for paintings. Large paintings are left out by this check...
			if (e.getLocation().getBlock().getRelative(attachedFace).equals(block)) {
				entities.add(hangingEntity);
			}
		}
		
		return entities;
		
	}
	
	
	/**
	 * Is an entity a hanging type, attachable to a block.
	 * @param entity the entity to check for being a hanging type
	 * @return if an entity is a hanging type attachable to a block
	 */
	public static boolean isHangingEntity( Entity entity ){
		EntityType type = entity.getType();

        switch (type) {
            case ITEM_FRAME:
            case PAINTING:
                return true;
            default:
                return false;
        }
	}
	
	
	/**
	 * Gets the other block that is part of a double length block
     *
	 * @param block the block to get the sibling of
	 */
	public static Block getSiblingForDoubleLengthBlock(Block block){
		/**
		 * Handle special double-length blocks
		 */
        switch (block.getType()) {
            case ACACIA_DOOR:
            case BIRCH_DOOR:
            case DARK_OAK_DOOR:
            case JUNGLE_DOOR:
            case IRON_DOOR_BLOCK:
            case SPRUCE_DOOR:
            case WOODEN_DOOR:
                if(block.getData() == 8 || block.getData() == 9) {
                    return block.getRelative(BlockFace.DOWN);
                }
                break;
            case DOUBLE_PLANT:
                if(block.getData() == 10) {
                    return block.getRelative(BlockFace.DOWN);
                }
                break;
            case BED_BLOCK:
                Bed b = (Bed)block.getState().getData();
                if(b.isHeadOfBed()){
                    return block.getRelative(b.getFacing().getOppositeFace());
                }
                break;
            case CHEST:
                return findFirstSurroundingBlockOfType(block, block.getType());
            case TRAPPED_CHEST:
                return findFirstSurroundingBlockOfType(block, block.getType());
        }

        return null;
	}
	
	
//	/**
//	 * 
//	 * @param mat
//	 * @param loc
//	 * @param radius
//	 */
//	public static ArrayList<BlockStateChange> removeMaterialFromRadius(Material mat, Location loc, int radius){
//		Material[] materials = { mat };
//		return removeMaterialsFromRadius(materials, loc, radius);
//	}
//	
//	
//	/**
//	 * 
//	 * @param mat
//	 * @param loc
//	 * @param radius
//	 */
//	public static ArrayList<BlockStateChange> removeMaterialsFromRadius(Material[] materials, Location loc, int radius){
//		ArrayList<BlockStateChange> blockStateChanges = new ArrayList<BlockStateChange>();
//		if(loc != null && radius > 0 && materials != null && materials.length > 0){
//			int x1 = loc.getBlockX();
//			int y1 = loc.getBlockY();
//			int z1 = loc.getBlockZ();
//			World world = loc.getWorld();
//			for(int x = x1-radius; x <= x1+radius; x++){
//				for(int y = y1-radius; y <= y1+radius; y++){
//					for(int z = z1-radius; z <= z1+radius; z++){
//						loc = new Location(world, x, y, z);
//						Block b = loc.getBlock();
//						if(b.getType().equals(Material.AIR)) continue;
//						if( Arrays.asList(materials).contains(loc.getBlock().getType()) ){
//							BlockState originalBlock = loc.getBlock().getState();
//							loc.getBlock().setType(Material.AIR);
//							BlockState newBlock = loc.getBlock().getState();
//							blockStateChanges.add(new BlockStateChange(originalBlock,newBlock));
//						}
//					}
//				}
//			}
//		}
//		return blockStateChanges;
//	}
//	
//	
//	/**
//	 * Extinguish all the fire in a radius
//	 * @param loc The location you want to extinguish around
//	 * @param radius The radius around the location you are extinguish
//	 */
//	public static ArrayList<BlockStateChange> extinguish(Location loc, int radius){
//		return removeMaterialFromRadius(Material.FIRE, loc, radius);
//	}
//	
//	
//	/**
//	 * Drains lava and water within (radius) around (loc).
//	 * @param loc
//	 * @param radius
//	 */
//	public static ArrayList<BlockStateChange> drain(Location loc, int radius){
//		Material[] materials = { Material.LAVA, Material.STATIONARY_LAVA, Material.WATER, Material.STATIONARY_WATER };
//		return removeMaterialsFromRadius(materials, loc, radius);
//	}
//	
//	
//	/**
//	 * Drains lava blocks (radius) around player's loc.
//	 * @param loc
//	 * @param radius
//	 */
//	public static ArrayList<BlockStateChange> drainlava(Location loc, int radius){
//		Material[] materials = { Material.LAVA, Material.STATIONARY_LAVA };
//		return removeMaterialsFromRadius(materials, loc, radius);
//	}
//	
//	
//	/**
//	 * Drains water blocks (radius) around player's loc.
//	 * @param loc
//	 * @param radius
//	 */
//	public static ArrayList<BlockStateChange> drainwater(Location loc, int radius){
//		Material[] materials = { Material.WATER, Material.STATIONARY_WATER };
//		return removeMaterialsFromRadius(materials, loc, radius);
//	}
	
	
	/**
	 * Lower door halves get byte values based on which direction the front
	 * of the door is facing.
	 * 
	 * 0 = West
	 * 1 = North
	 * 2 = East
	 * 3 = South
	 * 
	 * The upper halves of both door types always have a value of 8.
	 * 
	 * @param originalBlock
	 * @param typeid
	 * @param subid
	 */
	public static void properlySetDoor( Block originalBlock, int typeid, byte subid ){
		// Wood door upper or iron door upper
		if( subid == 8 || subid == 9 ){ // 8 for single doors or left side of double, 9 for right side of double
			Block aboveOrBelow = originalBlock.getRelative(BlockFace.DOWN);
			aboveOrBelow.setTypeId( typeid );
			aboveOrBelow.setData( (byte)0 ); // we have no way to know which direction the lower half was facing
		}
		// Wood door lower or iron door lower
		else {
			Block aboveOrBelow = originalBlock.getRelative(BlockFace.UP);
			aboveOrBelow.setTypeId( typeid );
			// Determine the directing the bottom half is facing, then check
			// it's left side for an existing door, because the subid changes
			// if we're on the right.
			Block left = null;
			switch(subid){
				case 0:
					// Back faces east
					left = originalBlock.getRelative(BlockFace.NORTH);
					break;
				case 1:
					// Back faces south
					left = originalBlock.getRelative(BlockFace.EAST);
					break;
				case 2:
					// Back faces west
					left = originalBlock.getRelative(BlockFace.SOUTH);
					break;
				case 3:
					// Back faces north
					left = originalBlock.getRelative(BlockFace.WEST);
					break;
			}
			if(aboveOrBelow != null){
				if( left != null && isDoor(left.getType()) ){
					aboveOrBelow.setData( (byte)9 );
				} else {
					aboveOrBelow.setData( (byte)8 );
				}
			}
		}
	}
	
	
	/**
	 * Checks if material given is the material of a door
     *
	 * @param m the material to check for being a door material
	 * @return whether the material is a door material
	 */
	public static boolean isDoor(Material m){
		switch(m){
            case ACACIA_DOOR:
            case BIRCH_DOOR:
            case DARK_OAK_DOOR:
            case JUNGLE_DOOR:
            case IRON_DOOR:
            case IRON_DOOR_BLOCK:
            case SPRUCE_DOOR:
			case WOOD_DOOR:
			case WOODEN_DOOR:
				return true;
			default:
				return false;
		}
	}
	
	
	/**
	 * Given the lower block of a bed, we translate that to the top
	 * half, figuring out which direction and data value it gets.
	 * 
	 * @param originalBlock
	 * @param typeid
	 * @param subid
	 */
	public static void properlySetBed( Block originalBlock, int typeid, byte subid ){
		Block top = null;
		int new_subid = 0;
		switch(subid){
			case 3:
				top = originalBlock.getRelative(BlockFace.EAST);
				new_subid = 11;
				break;
			case 2:
				top = originalBlock.getRelative(BlockFace.NORTH);
				new_subid = 10;
				break;
			case 1:
				top = originalBlock.getRelative(BlockFace.WEST);
				new_subid = 9;
				break;
			case 0:
				top = originalBlock.getRelative(BlockFace.SOUTH);
				new_subid = 8;
				break;
		}
		if(top != null){
			top.setTypeId(typeid);
			top.setData((byte)new_subid);
		} else {
			System.out.println("Error setting bed: block top location was illegal. Data value: " + subid + " New data value: " + new_subid);
		}
	}
	
	
	/**
	 * Properly sets the second-tier of a double-block tall plant.
	 * 
	 * @param originalBlock
	 * @param typeid
	 * @param subid
	 */
	public static void properlySetDoublePlant( Block originalBlock, int typeid, byte subid ){
		if( !originalBlock.getType().equals(Material.DOUBLE_PLANT) ) return;
		Block above = originalBlock.getRelative(BlockFace.UP);
		if( !isAcceptableForBlockPlace( above.getType() ) ) return;
		// choose an acceptable subid
		if( typeid == 175 && subid < 8 ) subid = 8;
		above.setTypeId(typeid);
		above.setData((byte)subid);
	}
	
	
	/**
	 * 
	 * @param m
	 * @return
	 */
	public static boolean canFlowBreakMaterial(Material m){
		switch(m){
			case ACTIVATOR_RAIL:
			case BROWN_MUSHROOM:
			case CACTUS:
			case CARROT:
			case COCOA: // different from pop off list
			case DEAD_BUSH:
			case DETECTOR_RAIL:
			case DOUBLE_PLANT:
			case POTATO:
			case CROPS:
			case DIODE:
			case DIODE_BLOCK_OFF:
			case DIODE_BLOCK_ON:
			case FLOWER_POT:
			case IRON_DOOR:
			case IRON_DOOR_BLOCK:
			case LADDER: // different from pop off list
			case LEVER:
			case LONG_GRASS:
			case MELON_STEM:
			case NETHER_WARTS:
			case POWERED_RAIL:
			case PUMPKIN_STEM:
			case RAILS:
			case RED_MUSHROOM:
			case RED_ROSE:
			case REDSTONE:
			case REDSTONE_COMPARATOR_OFF:
			case REDSTONE_COMPARATOR_ON:
			case REDSTONE_TORCH_OFF:
			case REDSTONE_TORCH_ON:
			case REDSTONE_WIRE:
			case SAPLING:
			case SIGN:
			case SIGN_POST:
			case SKULL:
			case SUGAR_CANE_BLOCK:
			case STONE_PLATE:
			case TORCH:
			case TRIPWIRE:
			case TRIPWIRE_HOOK: // different from pop off list
			case VINE: // different from pop off list
			case WATER_LILY:
			case WHEAT:
			case WOOD_DOOR:
			case WOOD_PLATE:
			case WOODEN_DOOR:
			case YELLOW_FLOWER:
			case END_ROD:
				return true;
			default:
				return false;
		}
	}
	
	
	/**
	 * 
	 * @param m
	 * @return
	 */
	public static boolean materialRequiresSoil(Material m){
		switch(m){
			case CROPS:
			case WHEAT:
			case POTATO:
			case CARROT:
			case MELON_STEM:
			case PUMPKIN_STEM:
				return true;
			default:
				return false;
		}
	}


	/**
	 *
	 * @param type
	 * @param currBlock
	 * @param foundLocations
	 * @return
	 */
	public static ArrayList<Block> findConnectedBlocksOfType( Material type, Block currBlock, ArrayList<Location> foundLocations ) {
    	
    	ArrayList<Block> foundBlocks = new ArrayList<Block>();
    	
    	if(foundLocations == null){
    		foundLocations = new ArrayList<Location>();
    	}
        	
    	foundLocations.add(currBlock.getLocation());
    	
    	for(int x = -1; x <= 1; x++){
    		for(int z = -1; z <= 1; z++){
    			for(int y = -1; y <= 1; y++){
        			Block newblock = currBlock.getRelative(x, y, z);
        			// ensure it matches the type and wasn't already found
        			if( newblock.getType() == type && !foundLocations.contains(newblock.getLocation()) ){
        				foundBlocks.add(newblock);
        				ArrayList<Block> additionalBlocks = findConnectedBlocksOfType( type, newblock, foundLocations );
        				if(additionalBlocks.size() > 0){
        					foundBlocks.addAll(additionalBlocks);
        				}
        			}
        		}
    		}
    	}

        return foundBlocks;
        
    }
    
    
    /**
     * 
     * @param m
     * @param loc
     * @return
     */
    public static Block getFirstBlockOfMaterialBelow( Material m, Location loc ){
    	for(int y = (int) loc.getY(); y > 0; y--){
    		loc.setY( y );
    		if(loc.getBlock().getType().equals(m)){
    			return loc.getBlock();
    		}
    	}
    	return null;
    }
    
    
    /**
     * 
     * @param m
     * @return
     */
    public static boolean isGrowableStructure(Material m){
		switch(m){
			case LEAVES:
			case LOG:
			case HUGE_MUSHROOM_1:
			case HUGE_MUSHROOM_2:
				return true;
			default:
				return false;
		}
	}
    
    
    /**
     * There are several items that are officially different
     * ItemStacks, but for the purposes of what we're doing
     * are really considered one core item. This attempts
     * to be a little lenient on matching the ids.
     * 
     * Example: Redstone lamp (off) is 123, (on) is 124 but 
     * either id means it's a redstone lamp.
     * 
     * @param id1
     * @param id2
     * @return
     */
    public static boolean areBlockIdsSameCoreItem( int id1, int id2 ){
    	
    	// Get the obvious one out of the way.
    	if(id1 == id2) return true;
    	
    	// Grass/Dirt
    	if( (id1 == 2 || id1 == 3) && (id2 == 2 || id2 == 3) ){
    		return true;
    	}
    	
    	// Mycel/Dirt
    	if( (id1 == 110 || id1 == 3) && (id2 == 110 || id2 == 3) ){
    		return true;
    	}
    	
    	// Water
    	if( (id1 == 8 || id1 == 9) && (id2 == 8 || id2 == 9) ){
    		return true;
    	}
    	
    	// Lava
    	if( (id1 == 10 || id1 == 11) && (id2 == 10 || id2 == 11) ){
    		return true;
    	}
    	
    	// Redstone torch
    	if( (id1 == 75 || id1 == 76) && (id2 == 75 || id2 == 76) ){
    		return true;
    	}
    	
    	// Repeater
    	if( (id1 == 93 || id1 == 94) && (id2 == 93 || id2 == 94) ){
    		return true;
    	}
    	
    	// Redstone lamp
    	if( (id1 == 123 || id1 == 124) && (id2 == 123 || id2 == 124) ){
    		return true;
    	}
    	
    	// Furnace
    	if( (id1 == 61 || id1 == 62) && (id2 == 61 || id2 == 62) ){
    		return true;
    	}
    	
    	// Redstone comparator
    	if( (id1 == 149 || id1 == 150) && (id2 == 149 || id2 == 150) ){
    		return true;
    	}
    	
    	return false;
    	
    }
}
