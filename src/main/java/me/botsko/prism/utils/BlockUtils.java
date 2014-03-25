package me.botsko.prism.utils;

import java.util.ArrayList;
import java.util.Arrays;

import me.botsko.prism.events.BlockStateChange;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;

public class BlockUtils extends me.botsko.elixr.BlockUtils {

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
    public static int blockIdMustRecordAs(int block_id) {
        // Burning Furnace -> Furnace
        if( block_id == 62 ) { return 61; }
        return block_id;
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
}
