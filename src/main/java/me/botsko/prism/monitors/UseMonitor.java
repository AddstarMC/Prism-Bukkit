package me.botsko.prism.monitors;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import me.botsko.prism.utils.MiscUtils;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.ChatColor;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;

public class UseMonitor {

	/**
	 * 
	 */
	private final Prism plugin;

	/**
	 * 
	 */
	protected final List<String> blocksToAlertOnPlace;

	/**
	 * 
	 */
	protected final List<String> blocksToAlertOnBreak;

	/**
	 * 
	 */
	private ConcurrentHashMap<String, Integer> countedEvents = new ConcurrentHashMap<>();

	/**
	 * 
	 * @param plugin
	 */
	public UseMonitor(Prism plugin) {
		this.plugin = plugin;
		blocksToAlertOnPlace = plugin.getConfig().getStringList("prism.alerts.uses.item-placement");
		blocksToAlertOnPlace.replaceAll(String::toUpperCase);
		blocksToAlertOnBreak = plugin.getConfig().getStringList("prism.alerts.uses.item-break");
		blocksToAlertOnBreak.replaceAll(String::toUpperCase);
		resetEventsQueue();
	}

	/**
	 * 
	 * @param playername
	 * @return
	 */
	protected void incrementCount(String playername, String msg) {

		int count = 0;
		if (countedEvents.containsKey(playername)) {
			count = countedEvents.get(playername);
		}
		count++;
		countedEvents.put(playername, count);

		msg = ChatColor.GRAY + playername + " " + msg;
		if (count == 5) {
			msg = playername + " continues - pausing warnings.";
		}

		if (count <= 5) {
			if (plugin.getConfig().getBoolean("prism.alerts.uses.log-to-console")) {
				plugin.alertPlayers(null, msg);
				Prism.log(TypeUtils.colorize(msg));
			}

			// Log to commands
			List<String> commands = plugin.getConfig().getStringList("prism.alerts.uses.log-commands");
			MiscUtils.dispatchAlert(msg, commands);
		}
	}

	/**
	 * 
	 * @param player
	 * @return
	 */
	protected boolean checkFeatureShouldProceed(Player player) {

		// Ensure enabled
		if (!plugin.getConfig().getBoolean("prism.alerts.uses.enabled"))
			return false;

		// Ignore players who would see the alerts
		if (plugin.getConfig().getBoolean("prism.alerts.uses.ignore-staff") && player.hasPermission("prism.alerts"))
			return false;

		// Ignore certain ranks
		return !player.hasPermission("prism.bypass-use-alerts");
	}

	/**
	 * 
	 * @param player
	 * @param block
	 */
	public void alertOnBlockPlacement(Player player, Block block) {

		// Ensure enabled
		if (!checkFeatureShouldProceed(player))
			return;

		final String playername = player.getName();
		final String blockType = "" + block.getType();

		// Ensure we're tracking this block
		if (blocksToAlertOnPlace.contains(blockType) || blocksToAlertOnPlace.contains(block.getType().name())) {
			final String alias = Prism.getItems().getAlias(block.getType(), block.getBlockData());
			incrementCount(playername, "placed " + alias);
		}
	}

	/**
	 * 
	 * @param player
	 * @param block
	 */
	public void alertOnBlockBreak(Player player, Block block) {

		// Ensure enabled
		if (!checkFeatureShouldProceed(player))
			return;

		final String playername = player.getName();
		final String blockType = "" + block.getType();

		// Ensure we're tracking this block
		if (blocksToAlertOnBreak.contains(blockType) || blocksToAlertOnBreak.contains(block.getType().name())) {
			final String alias = Prism.getItems().getAlias(block.getType(), block.getBlockData());
			incrementCount(playername, "broke " + alias);
		}
	}

	/**
	 * 
	 * @param player
	 * @param use_msg
	 */
	public void alertOnItemUse(Player player, String use_msg) {

		// Ensure enabled
		if (!checkFeatureShouldProceed(player))
			return;

		final String playername = player.getName();
		incrementCount(playername, use_msg);

	}

	/**
	 * 
	 * @param player
	 * @param use_msg
	 */
	public void alertOnVanillaXray(Player player, String use_msg) {

		if (!checkFeatureShouldProceed(player))
			return;

		final String playername = player.getName();
		incrementCount(playername, use_msg);

	}

	/**
	 * Reset the queue every now and then Technically this can reset someone's
	 * counts too early but that just means staff will see extra warnings.
	 */
	public void resetEventsQueue() {
		plugin.getServer().getScheduler().scheduleSyncRepeatingTask(plugin, () -> countedEvents = new ConcurrentHashMap<>(), 7000L, 7000L);
	}
}