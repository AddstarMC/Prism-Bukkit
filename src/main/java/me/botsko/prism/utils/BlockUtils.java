package me.botsko.prism.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;

import me.botsko.prism.events.BlockStateChange;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.material.Bed;
import org.bukkit.material.Door;

public class BlockUtils {
	
	// TODO: 1.13 material change
	private static EnumSet<Material> replaceableMaterials = EnumSet.of(
			Material.AIR,
			Material.FIRE,
			Material.GRAVEL,
			Material.LAVA,
			Material.LONG_GRASS,
			Material.SAND,
			Material.SNOW,
			Material.SNOW_BLOCK,
			Material.STATIONARY_LAVA,
			Material.STATIONARY_WATER,
			Material.WATER);
	
	private static EnumSet<Material> fallingMaterials = EnumSet.of(
			Material.SAND,
			Material.GRAVEL,
			Material.ANVIL,
			Material.DRAGON_EGG,
			Material.CONCRETE_POWDER);

	private static EnumSet<Material> fallsOffWall = EnumSet.of(
			Material.POWERED_RAIL,
			Material.DETECTOR_RAIL,
			Material.PISTON_STICKY_BASE,
			Material.PISTON_BASE,
			Material.PISTON_EXTENSION,
			Material.PISTON_MOVING_PIECE,
			Material.TORCH,
			Material.LADDER,
			Material.RAILS,
			Material.WALL_SIGN,
			Material.LEVER,
			Material.REDSTONE_TORCH_OFF,
			Material.REDSTONE_TORCH_ON,
			Material.STONE_BUTTON,
			Material.PORTAL,
			Material.VINE,
			Material.COCOA,
			Material.TRIPWIRE_HOOK,
			Material.WOOD_BUTTON,
			Material.ACTIVATOR_RAIL);
	
	private static EnumSet<Material> fallsOffTop = EnumSet.of(
			Material.SAPLING,
			Material.POWERED_RAIL,
			Material.DETECTOR_RAIL,
			Material.PISTON_STICKY_BASE,
			Material.LONG_GRASS,
			Material.DEAD_BUSH,
			Material.PISTON_BASE,
			Material.PISTON_EXTENSION,
			Material.PISTON_MOVING_PIECE,
			Material.YELLOW_FLOWER,
			Material.RED_ROSE,
			Material.BROWN_MUSHROOM,
			Material.RED_MUSHROOM,
			Material.TORCH,
			Material.REDSTONE,
			Material.CROPS,
			Material.WOODEN_DOOR,
			Material.RAILS,
			Material.SIGN_POST,
			Material.LEVER,
			Material.STONE_PLATE,
			Material.IRON_DOOR_BLOCK,
			Material.WOOD_PLATE,
			Material.REDSTONE_TORCH_OFF,
			Material.REDSTONE_TORCH_ON,
			Material.STONE_BUTTON,
			Material.SNOW,
			Material.CACTUS,
			Material.SUGAR_CANE_BLOCK,
			Material.PORTAL,
			Material.DIODE_BLOCK_OFF,
			Material.DIODE_BLOCK_ON,
			Material.PUMPKIN_STEM,
			Material.MELON_STEM,
			Material.WATER_LILY,
			Material.NETHER_WARTS,
			Material.FLOWER_POT,
			Material.CARROT,
			Material.POTATO,
			Material.WOOD_BUTTON,
			Material.GOLD_PLATE,
			Material.IRON_PLATE,
			Material.REDSTONE_COMPARATOR_OFF,
			Material.REDSTONE_COMPARATOR_ON,
			Material.ACTIVATOR_RAIL,
			Material.CARPET,
			Material.DOUBLE_PLANT,
			Material.STANDING_BANNER,
			Material.SPRUCE_DOOR,
			Material.BIRCH_DOOR,
			Material.JUNGLE_DOOR,
			Material.ACACIA_DOOR,
			Material.DARK_OAK_DOOR,
			Material.BEETROOT_BLOCK);
	
	private static EnumSet<Material> detachingBlocks = EnumSet.of(
			Material.AIR,
			Material.FIRE,
			Material.WATER,
			Material.STATIONARY_WATER,
			Material.LAVA,
			Material.STATIONARY_LAVA);

    /**
     * There are some blocks that are broken in an "on" state. Rather than
     * record the on state we need to record the off state, something that is
     * available to players.
     * 
     * Example: Player breaks a furnce mid-smelt. The block broken is
     * "Burning Furance #62", which means we'll restore block 62 which was not
     * meant to be permanent.
     * 
     * 
     * @param block_id
     * @return
     */
    public static Material blockIdMustRecordAs(Material material) {
        // Burning Furnace -> Furnace
        if( material == Material.BURNING_FURNACE ) { return Material.FURNACE; }
        return material;
    }

    /**
     * /**
     * 
     * @param mat
     * @param loc
     * @param radius
     */
    public static ArrayList<BlockStateChange> removeMaterialFromRadius(Material mat, Location loc, int radius) {
        final Material[] materials = { mat };
        return removeMaterialsFromRadius( materials, loc, radius );
    }

    /**
     * 
     * @param materials
     * @param loc
     * @param radius
     */
    public static ArrayList<BlockStateChange> removeMaterialsFromRadius(Material[] materials, Location loc, int radius) {
        final ArrayList<BlockStateChange> blockStateChanges = new ArrayList<BlockStateChange>();
        if( loc != null && radius > 0 && materials != null && materials.length > 0 ) {
            final int x1 = loc.getBlockX();
            final int y1 = loc.getBlockY();
            final int z1 = loc.getBlockZ();
            final World world = loc.getWorld();
            for ( int x = x1 - radius; x <= x1 + radius; x++ ) {
                for ( int y = y1 - radius; y <= y1 + radius; y++ ) {
                    for ( int z = z1 - radius; z <= z1 + radius; z++ ) {
                        loc = new Location( world, x, y, z );
                        final Block b = loc.getBlock();
                        if( b.getType().equals( Material.AIR ) )
                            continue;
                        if( Arrays.asList( materials ).contains( loc.getBlock().getType() ) ) {
                            final BlockState originalBlock = loc.getBlock().getState();
                            loc.getBlock().setType( Material.AIR );
                            final BlockState newBlock = loc.getBlock().getState();
                            blockStateChanges.add( new BlockStateChange( originalBlock, newBlock ) );
                        }
                    }
                }
            }
        }
        return blockStateChanges;
    }

    /**
     * Extinguish all the fire in a radius
     * 
     * @param loc
     *            The location you want to extinguish around
     * @param radius
     *            The radius around the location you are extinguish
     */
    public static ArrayList<BlockStateChange> extinguish(Location loc, int radius) {
        return removeMaterialFromRadius( Material.FIRE, loc, radius );
    }

    /**
     * Drains lava and water within (radius) around (loc).
     * 
     * @param loc
     * @param radius
     */
    public static ArrayList<BlockStateChange> drain(Location loc, int radius) {
        final Material[] materials = { Material.LAVA, Material.STATIONARY_LAVA, Material.WATER,
                Material.STATIONARY_WATER };
        return removeMaterialsFromRadius( materials, loc, radius );
    }

    /**
     * Drains lava blocks (radius) around player's loc.
     * 
     * @param loc
     * @param radius
     */
    public static ArrayList<BlockStateChange> drainlava(Location loc, int radius) {
        final Material[] materials = { Material.LAVA, Material.STATIONARY_LAVA };
        return removeMaterialsFromRadius( materials, loc, radius );
    }

    /**
     * Drains water blocks (radius) around player's loc.
     * 
     * @param loc
     * @param radius
     */
    public static ArrayList<BlockStateChange> drainwater(Location loc, int radius) {
        final Material[] materials = { Material.WATER, Material.STATIONARY_WATER };
        return removeMaterialsFromRadius( materials, loc, radius );
    }
    

	/**
	 * Determines if the material of an existing block at a location is
	 * something that's commonly acceptable to replace.
	 * 
	 * @param m the material of the block
	 * @return if the material is acceptable to replace
	 */
	public static boolean isAcceptableForBlockPlace( Material m ){
		return replaceableMaterials.contains(m);
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
	 * @param block the block to check for the ability to fall
	 * @return whether the block is capable of falling
	 */
	public static boolean isFallingBlock( Block block ){
		return fallingMaterials.contains(block.getType());
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
		return fallsOffWall.contains(m);
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
		return fallsOffTop.contains(m);
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
		return detachingBlocks.contains(m);
	}
	
	
	/**
	 * Searches for detachable entities in a
	 * 
	 * @param block
	 * @return
	 */
	public static ArrayList<Entity> findHangingEntities( final Block block ){
		
		ArrayList<Entity> entities = new ArrayList<Entity>();
		
		Entity[] foundEntities = block.getChunk().getEntities();
		if(foundEntities.length > 0){
			for(Entity e : foundEntities){
				// Some modded servers seems to list entities in the chunk
				// that exists in other worlds. No idea why but we can at
				// least check for it.
				// https://snowy-evening.com/botsko/prism/318/
				if( !block.getWorld().equals( e.getWorld() ) ) continue;
				// Let's limit this to only entities within 1 block of the current.
				if( block.getLocation().distance( e.getLocation() ) < 2 && isHangingEntity(e) ){
					entities.add(e);
				}
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
		/*MaterialData data = block.getState().getData();
		
		if(data instanceof Door) {
			((Door)data).isTopHalf();
			return block.getRelative(BlockFace.DOWN);
		}
		if(data instanceof )*/
		
        switch (block.getType()) {
            case ACACIA_DOOR:
            case BIRCH_DOOR:
            case DARK_OAK_DOOR:
            case JUNGLE_DOOR:
            case IRON_DOOR_BLOCK:
            case SPRUCE_DOOR:
            case WOODEN_DOOR:
            	if(((Door)block.getState().getData()).isTopHalf()) {
            		return block.getRelative(BlockFace.DOWN);
            	}
                break;
            case DOUBLE_PLANT:
            	//TODO: 1.13 
            	@SuppressWarnings("deprecation")
            	byte data = block.getData();
                if(data == 10) {
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
            default:
                break;
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
	public static void properlySetDoor( Block originalBlock, Material typeid, byte subid ){
		// Wood door upper or iron door upper
		if( subid == 8 || subid == 9 ){ // 8 for single doors or left side of double, 9 for right side of double
			Block aboveOrBelow = originalBlock.getRelative(BlockFace.DOWN);
			aboveOrBelow.setType( typeid );
			aboveOrBelow.setData( (byte)0 ); // we have no way to know which direction the lower half was facing
		}
		// Wood door lower or iron door lower
		else {
			Block aboveOrBelow = originalBlock.getRelative(BlockFace.UP);
			aboveOrBelow.setType( typeid );
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
	public static void properlySetBed( Block originalBlock, Material typeid, byte subid ){
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
			top.setType(typeid);
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
	public static void properlySetDoublePlant( Block originalBlock, Material typeid, byte subid ){
		if( !originalBlock.getType().equals(Material.DOUBLE_PLANT) ) return;
		Block above = originalBlock.getRelative(BlockFace.UP);
		if( !isAcceptableForBlockPlace( above.getType() ) ) return;
		// choose an acceptable subid
		if( typeid == Material.DOUBLE_PLANT && subid < 8 ) subid = 8;
		above.setType(typeid);
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
	 * @param currBlock
	 * @param toBeFelled
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
