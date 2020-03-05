package me.botsko.prism.monitors;

import java.util.ArrayList;
import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.utils.MiscUtils;
import me.botsko.prism.utils.TypeUtils;

import org.bukkit.GameMode;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.Player;

public class OreMonitor {

	/**
	 * 
	 */
	private final int threshold_max = 100;

	/**
	 * 
	 */
	private int threshold = 1;

	/**
	 * 
	 */
	private final Prism plugin;

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
	public OreMonitor(Prism plugin) {
		this.plugin = plugin;
	}

	/**
	 * 
	 * @param player
	 * @param block
	 */
	public void processAlertsFromBlock(final Player player, final Block block) {

		if (!plugin.getConfig().getBoolean("prism.alerts.ores.enabled")) {
			return;
		}

		if (player == null || player.getGameMode() == null || player.getGameMode().equals(GameMode.CREATIVE)) {
			return;
		}

		if (block != null && isWatched(block) && !plugin.alertedBlocks.containsKey(block.getLocation())) {

			threshold = 1;

			// identify all ore blocks on same Y axis in x/z direction
			final ArrayList<Block> matchingBlocks = new ArrayList<>();
			final ArrayList<Block> foundores = findNeighborBlocks(block.getType(), block, matchingBlocks);
			if (!foundores.isEmpty()) {

				// Determine light level
				int light = 0;
				final BlockFace[] blockFaces = {BlockFace.NORTH, BlockFace.SOUTH, BlockFace.EAST, BlockFace.WEST, BlockFace.UP, BlockFace.DOWN};
				for (BlockFace blockFace: blockFaces) {
					light = Math.max(light, block.getRelative(blockFace).getLightLevel());
					if (light >= 15) {
						break;
					}
				}
				light = light * 100 / 15;

				// Create alert message
				final String count = foundores.size() + (foundores.size() >= threshold_max ? "+" : "");
				final String msg = getOreColor(block) + player.getName() + " found " + count + " "
						+ getOreNiceName(block) + " " + light + "% light";

				/*
				  Run the lookup itself in an async task so the lookup query isn't done on the
				  main thread
				 */
				plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> {

					// check if block placed
					boolean wasplaced = false;

					// Build params
					final QueryParameters params = new QueryParameters();
					params.setWorld(player.getWorld().getName());
					params.addSpecificBlockLocation(block.getLocation());
					params.addActionType("block-place");

					final ActionsQuery aq = new ActionsQuery(plugin);
					final QueryResult results = aq.lookup(params, player);
					if (!results.getActionResults().isEmpty()) {
						wasplaced = true;
					}

					if (!wasplaced) {

						// Alert staff
						plugin.alertPlayers(null, TypeUtils.colorize(msg));

						// Log to console
						if (plugin.getConfig().getBoolean("prism.alerts.ores.log-to-console")) {
							Prism.log(TypeUtils.colorize(msg));
						}

						// Log to commands
						List<String> commands = plugin.getConfig().getStringList("prism.alerts.ores.log-commands");
						MiscUtils.dispatchAlert(msg, commands);
					}
				});
			}
		}
	}

	/**
	 * 
	 * @param block
	 * @return
	 */
	protected String getOreColor(Block block) {
		if (isWatched(block)) {
			return Prism.getAlertedOres().get("" + block.getType());
		}
		else {
			return "&f";
		}
	}

	/**
	 * 
	 * @param block
	 * @return
	 */
	protected String getOreNiceName(Block block) {
		return block.getType().toString().replace("_", " ").toLowerCase().replace("glowing", " ");
	}

	/**
	 * 
	 * @param block
	 * @return
	 */
	protected boolean isWatched(Block block) {
		return Prism.getAlertedOres().containsKey("" + block.getType().name());
	}

	/**
	 * @param type
	 * @param currBlock
	 * @param matchingBlocks
	 */
	private ArrayList<Block> findNeighborBlocks(Material type, Block currBlock, ArrayList<Block> matchingBlocks) {

		if (isWatched(currBlock)) {

			matchingBlocks.add(currBlock);
			final java.util.Date date = new java.util.Date();
			plugin.alertedBlocks.put(currBlock.getLocation(), date.getTime());

			for (int x = -1; x <= 1; x++) {
				for (int z = -1; z <= 1; z++) {
					for (int y = -1; y <= 1; y++) {
						final Block newblock = currBlock.getRelative(x, y, z);
						// ensure it matches the type and wasn't already found
						if (newblock.getType() == type && !matchingBlocks.contains(newblock)) {
							threshold++;
							if (threshold <= threshold_max) {
								findNeighborBlocks(type, newblock, matchingBlocks);
							}
						}
					}
				}
			}
		}

		return matchingBlocks;

	}
}
