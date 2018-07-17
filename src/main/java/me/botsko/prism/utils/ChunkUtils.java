package me.botsko.prism.utils;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.Chunk;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;
import org.bukkit.util.Vector;

public class ChunkUtils {

	/**
	 * Resets the preview border blocks
	 * 
	 * @param player
	 * @param blocks
	 */
	public static void resetPreviewBoundaryBlocks(Player player, List<Block> blocks) {
		for (Block block : blocks) {
			// TODO: Check for non-deprecated method in future
			byte data = block.getData();

			EntityUtils.sendBlockChange(player, block.getLocation(), block.getType(), data);
		}
	}

	/**
	 * Sets a block in preview mode around the border of a chunk
	 * 
	 * @param player
	 * @param blocks
	 */
	public static void setPreviewBoundaryBlocks(Player player, List<Block> blocks, Material m) {
		for (Block block : blocks) {
			EntityUtils.sendBlockChange(player, block.getLocation(), m, 0);
		}
	}

	/**
	 * Returns the minimum vector for the chunk
	 * 
	 * @param chunk
	 * @return
	 */
	public static Vector getChunkMinVector(Chunk chunk) {
		int blockMinX = chunk.getX() * 16;
		int blockMinZ = chunk.getZ() * 16;
		return new Vector(blockMinX, 0, blockMinZ);
	}

	/**
	 * Returns the maximum vector for the chunk
	 * 
	 * @param chunk
	 * @return
	 */
	public static Vector getChunkMaxVector(Chunk chunk) {
		int blockMinX = chunk.getX() * 16;
		int blockMinZ = chunk.getZ() * 16;
		int blockMaxX = blockMinX + 15;
		int blockMaxZ = blockMinZ + 15;
		return new Vector(blockMaxX, chunk.getWorld().getMaxHeight(), blockMaxZ);
	}

	/**
	 * Returns an array of boundary blocks at a single Y for the current chunk
	 * 
	 * @param chunk
	 * @return
	 */
	public static ArrayList<Block> getBoundingBlocksAtY(Chunk chunk, int y) {

		int blockMinX = chunk.getX() * 16;
		int blockMinZ = chunk.getZ() * 16;
		int blockMaxX = blockMinX + 15;
		int blockMaxZ = blockMinZ + 15;

		ArrayList<Block> blocks = new ArrayList<Block>();

		// MinX -> MinZ
		for (int x = blockMinX; x < blockMaxX; x++) {
			blocks.add(chunk.getWorld().getBlockAt(x, y, blockMinZ));
		}
		// MinX -> MaxZ
		for (int x = blockMinX; x < blockMaxX; x++) {
			blocks.add(chunk.getWorld().getBlockAt(x, y, blockMaxZ));
		}
		// MinZ -> MinX
		for (int z = blockMinZ; z < blockMaxZ; z++) {
			blocks.add(chunk.getWorld().getBlockAt(blockMinX, y, z));
		}
		// MinZ -> MaxX
		for (int z = blockMinZ; z < blockMaxZ; z++) {
			blocks.add(chunk.getWorld().getBlockAt(blockMaxX, y, z));
		}

		return blocks;

	}
}