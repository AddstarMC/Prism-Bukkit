package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.players.PlayerIdentification;
import me.botsko.prism.utils.InventoryUtils;
import me.botsko.prism.utils.MaterialTag;
import me.botsko.prism.utils.MiscUtils;
import me.botsko.prism.wands.ProfileWand;
import me.botsko.prism.wands.Wand;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.Jukebox;
import org.bukkit.block.data.BlockData;
import org.bukkit.block.data.Waterlogged;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.Event.Result;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.enchantment.EnchantItemEvent;
import org.bukkit.event.entity.EntityPickupItemEvent;
import org.bukkit.event.inventory.CraftItemEvent;
import org.bukkit.event.player.*;
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;

import java.util.List;

public class PrismPlayerEvents implements Listener {

	/**
	 * 
	 */
	private final Prism plugin;

	/**
	 * 
	 */
	private final List<String> illegalCommands;

	/**
	 * 
	 */
	private final List<String> ignoreCommands;

	/**
	 * 
	 * @param plugin
	 */
	public PrismPlayerEvents(Prism plugin) {
		this.plugin = plugin;
		illegalCommands = plugin.getConfig().getStringList("prism.alerts.illegal-commands.commands");
		ignoreCommands = plugin.getConfig().getStringList("prism.do-not-track.commands");
	}

	/**
	 * Log command use
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCommandPreprocess(PlayerCommandPreprocessEvent event) {

		final Player player = event.getPlayer();
		final String cmd = event.getMessage().toLowerCase();

		final String[] cmdArgs = cmd.split(" ");
		final String primaryCmd = cmdArgs[0].substring(1);

		if (plugin.getConfig().getBoolean("prism.alerts.illegal-commands.enabled")) {
			if (illegalCommands.contains(primaryCmd)) {
				final String msg = player.getName() + " attempted an illegal command: " + primaryCmd + ". Originally: "
						+ cmd;
				player.sendMessage(Prism.messenger.playerError("Sorry, this command is not available in-game."));
				plugin.alertPlayers(null, msg);
				event.setCancelled(true);
				// Log to console
				if (plugin.getConfig().getBoolean("prism.alerts.illegal-commands.log-to-console")) {
					Prism.log(msg);
				}

				// Log to commands
				List<String> commands = plugin.getConfig().getStringList("prism.alerts.illegal-commands.log-commands");
				MiscUtils.dispatchAlert(msg, commands);
			}
		}

		if (!Prism.getIgnore().event("player-command", player))
			return;

		// Ignore some commands based on config
		if (ignoreCommands.contains(primaryCmd)) {
			return;
		}

		RecordingQueue.addToQueue(ActionFactory.createPlayer("player-command", player, event.getMessage()));

	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerJoin(final PlayerJoinEvent event) {

		final Player player = event.getPlayer();

		// Lookup player for cache reasons
		PlayerIdentification.cachePrismPlayer(player);

		// Track the join event
		if (!Prism.getIgnore().event("player-join", player))
			return;

		String ip = null;
		if (plugin.getConfig().getBoolean("prism.track-player-ip-on-join")) {
			ip = player.getAddress().getAddress().getHostAddress();
		}

		RecordingQueue.addToQueue(ActionFactory.createPlayer("player-join", event.getPlayer(), ip));
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
	public void onPlayerQuit(final PlayerQuitEvent event) {

		// Remove from primary key cache
		Prism.prismPlayers.remove(event.getPlayer().getUniqueId());

		// Track player quit
		if (!Prism.getIgnore().event("player-quit", event.getPlayer()))
			return;

		RecordingQueue.addToQueue(ActionFactory.createPlayer("player-quit", event.getPlayer(), null));

		// Remove any active wands for this player
		if (Prism.playersWithActiveTools.containsKey(event.getPlayer().getName())) {
			Prism.playersWithActiveTools.remove(event.getPlayer().getName());
		}
		// Remove any active previews for this player, even though they would
		// expire
		// naturally.
		if (plugin.playerActivePreviews.containsKey(event.getPlayer().getName())) {
			plugin.playerActivePreviews.remove(event.getPlayer().getName());
		}
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerChat(final AsyncPlayerChatEvent event) {

		if (!Prism.getIgnore().event("player-chat", event.getPlayer()))
			return;

		if (plugin.dependencyEnabled("Herochat"))
			return;

		RecordingQueue.addToQueue(ActionFactory.createPlayer("player-chat", event.getPlayer(), event.getMessage()));
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerDropItem(final PlayerDropItemEvent event) {
		if (!Prism.getIgnore().event("item-drop", event.getPlayer()))
			return;
		RecordingQueue.addToQueue(ActionFactory.createItemStack("item-drop", event.getItemDrop().getItemStack(),
				event.getItemDrop().getItemStack().getAmount(), -1, null, event.getPlayer().getLocation(),
				event.getPlayer()));
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerPickupItem(final EntityPickupItemEvent event) {
		if (event.getEntity() instanceof Player) {
			Player p = (Player) event.getEntity();
			if (!Prism.getIgnore().event("item-pickup", p))
				return;
			RecordingQueue.addToQueue(ActionFactory.createItemStack("item-pickup", event.getItem().getItemStack(),
					event.getItem().getItemStack().getAmount(), -1, null, p.getLocation(), p));
		}
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerExpChangeEvent(final PlayerExpChangeEvent event) {
		if (!Prism.getIgnore().event("xp-pickup", event.getPlayer()))
			return;
		RecordingQueue.addToQueue(ActionFactory.createPlayer("xp-pickup", event.getPlayer(), "" + event.getAmount()));
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerBucketEmpty(final PlayerBucketEmptyEvent event) {

		final Player player = event.getPlayer();
		String cause;
		Material newMat;
		Block spot = event.getBlockClicked().getRelative(event.getBlockFace());

		if (event.getBucket() == Material.LAVA_BUCKET) {
			cause = "lava-bucket";
			newMat = Material.LAVA;
		}
		else {
			cause = "water-bucket";
			newMat = Material.WATER;

			if (event.getBucket() != Material.WATER_BUCKET) {
				// TODO: handle fish here maybe
			}
		}

		if (!Prism.getIgnore().event(cause, player))
			return;

		BlockData oldData = spot.getBlockData();
		BlockData newData = Bukkit.createBlockData(newMat);

		BlockData clickedData = event.getBlockClicked().getBlockData();

		// TODO If "Lavalogged" blocks become a thing, please revisit.
		if (clickedData instanceof Waterlogged && event.getBucket() != Material.LAVA) {
			Waterlogged wl = (Waterlogged) clickedData;

			if (!wl.isWaterlogged()) {
				spot = event.getBlockClicked();
				newMat = spot.getType();
				oldData = wl;

				newData = wl.clone();
				((Waterlogged) newData).setWaterlogged(true);

				cause = "water-bucket";
			}
		}

		RecordingQueue.addToQueue(ActionFactory.createBlockChange(cause, spot.getLocation(), spot.getType(), oldData,
				newMat, newData, player));

		if (plugin.getConfig().getBoolean("prism.alerts.uses.lava") && event.getBucket() == Material.LAVA_BUCKET
				&& !player.hasPermission("prism.alerts.use.lavabucket.ignore")
				&& !player.hasPermission("prism.alerts.ignore")) {
			plugin.useMonitor.alertOnItemUse(player, "poured lava");
		}
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerBucketFill(final PlayerBucketFillEvent event) {

		final Player player = event.getPlayer();
		if (!Prism.getIgnore().event("bucket-fill", player))
			return;
		final Block spot = event.getBlockClicked().getRelative(event.getBlockFace());

		String liquid_type = "milk";
		if (spot.getType() == Material.WATER) {
			liquid_type = "water";
		}
		else if (spot.getType() == Material.LAVA) {
			liquid_type = "lava";
		}

		final Handler pa = ActionFactory.createPlayer("bucket-fill", player, liquid_type);

		// Override the location with the area taken
		pa.setX(spot.getX());
		pa.setY(spot.getY());
		pa.setZ(spot.getZ());

		RecordingQueue.addToQueue(pa);

	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerTeleport(final PlayerTeleportEvent event) {
		if (!Prism.getIgnore().event("player-teleport", event.getPlayer()))
			return;
		final TeleportCause c = event.getCause();
		if (c.equals(TeleportCause.END_PORTAL) || c.equals(TeleportCause.NETHER_PORTAL)
				|| c.equals(TeleportCause.ENDER_PEARL)) {
			RecordingQueue.addToQueue(ActionFactory.createEntityTravel("player-teleport", event.getPlayer(),
					event.getFrom(), event.getTo(), event.getCause()));
		}
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEnchantItem(final EnchantItemEvent event) {
		if (!Prism.getIgnore().event("enchant-item", event.getEnchanter()))
			return;
		final Player player = event.getEnchanter();
		RecordingQueue.addToQueue(ActionFactory.createItemStack("enchant-item", event.getItem(),
				event.getEnchantsToAdd(), event.getEnchantBlock().getLocation(), player));
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCraftItem(final CraftItemEvent event) {
		final Player player = (Player) event.getWhoClicked();
		if (!Prism.getIgnore().event("craft-item", player))
			return;
		final ItemStack item = event.getRecipe().getResult();
		RecordingQueue.addToQueue(
				ActionFactory.createItemStack("craft-item", item, 1, -1, null, player.getLocation(), player));
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.HIGHEST)
	public void onPlayerInteract(final PlayerInteractEvent event) {

		final Player player = event.getPlayer();
		Block block = event.getClickedBlock();

		ItemStack hand = player.getInventory().getItemInMainHand();

		final Wand wand = Prism.playersWithActiveTools.get(player.getName());
		// Are they using a wand (or do we always allow it)
		if (wand != null) {

			// The wand will tell us what to use.
			final Material item_mat = wand.getItem();

			// Does the player have such item?
			if (hand.getType() == item_mat) {

				// Left click is for current block
				if (event.getAction() == Action.LEFT_CLICK_BLOCK && event.getHand() == EquipmentSlot.HAND) {
					wand.playerLeftClick(player, block.getLocation());
				}
				// Right click is for relative block on blockface
				// except block placements - those will be handled by the
				// blockplace.
				if (event.getAction() == Action.RIGHT_CLICK_BLOCK && event.getHand() == EquipmentSlot.HAND) {
					block = block.getRelative(event.getBlockFace());
					wand.playerRightClick(player, block.getLocation());
				}

				if ((event.getAction() == Action.RIGHT_CLICK_BLOCK || event.getAction() == Action.LEFT_CLICK_BLOCK)) {
					Prism.debug("Cancelling event for wand use.");
					event.setCancelled(true);
					InventoryUtils.updateInventory(player);
					return;
				}
			}
		}

		if (event.useInteractedBlock() == Result.DENY)
			return;

		// Doors, buttons, containers, etc may only be opened with a right-click
		// as of 1.4
		if (block != null && event.getAction() == Action.RIGHT_CLICK_BLOCK) {

			String coord_key;
			switch (block.getType()) {
				case FURNACE:
				case DISPENSER:
				case CHEST:
				case ENDER_CHEST:
				case ENCHANTING_TABLE:
				case ANVIL:
				case BREWING_STAND:
				case TRAPPED_CHEST:
				case HOPPER:
				case DROPPER:
					if (!Prism.getIgnore().event("container-access", player))
						return;
					RecordingQueue.addToQueue(ActionFactory.createBlock("container-access", block, player));
					break;
				case JUKEBOX:
					recordDiscInsert(block, player);
					break;
				case CAKE:
					recordCakeEat(block, player);
					break;
				case OAK_DOOR:
				case ACACIA_DOOR:
				case BIRCH_DOOR:
				case DARK_OAK_DOOR:
				case JUNGLE_DOOR:
				case SPRUCE_DOOR:
				case OAK_TRAPDOOR:
				case SPRUCE_TRAPDOOR:
				case BIRCH_TRAPDOOR:
				case JUNGLE_TRAPDOOR:
				case ACACIA_TRAPDOOR:
				case DARK_OAK_TRAPDOOR:
				case OAK_FENCE_GATE:
				case SPRUCE_FENCE_GATE:
				case BIRCH_FENCE_GATE:
				case JUNGLE_FENCE_GATE:
				case ACACIA_FENCE_GATE:
				case DARK_OAK_FENCE_GATE:
				case LEVER:
				case STONE_BUTTON:
				case OAK_BUTTON:
				case SPRUCE_BUTTON:
				case BIRCH_BUTTON:
				case JUNGLE_BUTTON:
				case ACACIA_BUTTON:
				case DARK_OAK_BUTTON:
					if (!Prism.getIgnore().event("block-use", player))
						return;
					RecordingQueue.addToQueue(ActionFactory.createBlock("block-use", block, player));
					break;
				case JUNGLE_LOG:
					recordCocoaPlantEvent(block, hand, event.getBlockFace(), player);
					break;
				case WHEAT:
				case GRASS:
				case MELON_STEM:
				case PUMPKIN_STEM:
				case OAK_SAPLING:
				case SPRUCE_SAPLING:
				case BIRCH_SAPLING:
				case JUNGLE_SAPLING:
				case ACACIA_SAPLING:
				case DARK_OAK_SAPLING:
				case CARROT:
				case POTATO:
					recordBonemealEvent(block, hand, event.getBlockFace(), player);
					break;
				case RAIL:
				case DETECTOR_RAIL:
				case POWERED_RAIL:
				case ACTIVATOR_RAIL:
					coord_key = block.getX() + ":" + block.getY() + ":" + block.getZ();
					plugin.preplannedVehiclePlacement.put(coord_key, player.getUniqueId().toString());
					break;
				case TNT:
					if (hand.getType().equals(Material.FLINT_AND_STEEL)) {
						if (!Prism.getIgnore().event("tnt-prime", player))
							return;
						RecordingQueue.addToQueue(ActionFactory.createUse("tnt-prime", hand.getType(), block, player));
					}
					break;
				default:
					break;
			}

			// if they're holding a spawner egg
			if (MaterialTag.SPAWN_EGGS.isTagged(hand.getType())) {
				recordMonsterEggUse(block, hand, player);
			}

			// if they're holding a rocket
			if (hand.getType() == Material.FIREWORK_ROCKET) {
				recordRocketLaunch(block, hand, event.getBlockFace(), player);
			}

			// if they're holding a boat (why they hell can you put boats on
			// anything...)
			if (MaterialTag.BOATS.isTagged(hand.getType())) {
				coord_key = block.getX() + ":" + (block.getY() + 1) + ":" + block.getZ();
				plugin.preplannedVehiclePlacement.put(coord_key, player.getUniqueId().toString());
			}
		}

		// Punching fire
		if (block != null && event.getAction() == Action.LEFT_CLICK_BLOCK) {
			final Block above = block.getRelative(BlockFace.UP);
			if (above.getType().equals(Material.FIRE)) {
				RecordingQueue.addToQueue(ActionFactory.createBlock("block-break", above, player));
			}
		}

		if (!plugin.getConfig().getBoolean("prism.tracking.crop-trample"))
			return;

		if (block != null && event.getAction() == Action.PHYSICAL) {
			if (block.getType() == Material.FARMLAND) { // They are stepping on
				// soil
				if (!Prism.getIgnore().event("crop-trample", player))
					return;
				RecordingQueue
						.addToQueue(ActionFactory.createBlock("crop-trample", block.getRelative(BlockFace.UP), player));
			}
		}
	}

	/**
	 * 
	 * @param block
	 * @param inhand
	 * @param player
	 */
	protected void recordCocoaPlantEvent(Block block, ItemStack inhand, BlockFace clickedFace, Player player) {
		if (!Prism.getIgnore().event("block-place", block))
			return;

		if (block.getType() == Material.JUNGLE_LOG && inhand.getType() == Material.COCOA_BEANS) {
			final Location newLoc = block.getRelative(clickedFace).getLocation();
			final Block actualBlock = block.getWorld().getBlockAt(newLoc);
			// This is a lame way to do this
			final BlockAction action = new BlockAction();
			action.setActionType("block-place");
			action.setPlayer(player);
			action.setUUID(player.getUniqueId());
			action.setLoc(actualBlock.getLocation());
			action.setMaterial(Material.COCOA);
			action.setBlockData(block.getBlockData());
			RecordingQueue.addToQueue(action);
		}
	}

	/**
	 * 
	 * @param block
	 * @param inhand
	 * @param clickedFace
	 * @param player
	 */
	protected void recordBonemealEvent(Block block, ItemStack inhand, BlockFace clickedFace, Player player) {
		if (inhand.getType() == Material.BONE_MEAL) {
			if (!Prism.getIgnore().event("bonemeal-use", block))
				return;
			RecordingQueue.addToQueue(ActionFactory.createUse("bonemeal-use", inhand.getType(), block, player));
		}
	}

	/**
	 * 
	 * @param block
	 * @param inhand
	 * @param player
	 */
	protected void recordMonsterEggUse(Block block, ItemStack inhand, Player player) {
		if (!Prism.getIgnore().event("spawnegg-use", block))
			return;
		RecordingQueue.addToQueue(ActionFactory.createUse("spawnegg-use",
				inhand.getType(), block, player));
	}

	/**
	 * 
	 * @param block
	 * @param inhand
	 * @param clickedFace
	 * @param player
	 */
	protected void recordRocketLaunch(Block block, ItemStack inhand, BlockFace clickedFace, Player player) {
		if (!Prism.getIgnore().event("firework-launch", block))
			return;
		RecordingQueue.addToQueue(
				ActionFactory.createItemStack("firework-launch", inhand, null, block.getLocation(), player));
	}

	/**
	 * 
	 * @param block
	 * @param player
	 */
	protected void recordCakeEat(Block block, Player player) {
		if (!Prism.getIgnore().event("cake-eat", block))
			return;
		RecordingQueue.addToQueue(ActionFactory.createUse("cake-eat", Material.CAKE, block, player));
	}

	/**
	 * 
	 * @param block
	 * @param player
	 */
	protected void recordDiscInsert(Block block, Player player) {
		ItemStack hand = player.getInventory().getItemInMainHand();
		// They have to be holding a record
		if (!hand.getType().isRecord())
			return;

		final Jukebox jukebox = (Jukebox) block.getState();

		// Do we have a disc inside? This will pop it out
		if (!jukebox.getPlaying().equals(Material.AIR)) {

			// Record currently playing disc
			final ItemStack i = new ItemStack(jukebox.getPlaying(), 1);
			RecordingQueue.addToQueue(ActionFactory.createItemStack("item-remove", i, i.getAmount(), 0, null,
					block.getLocation(), player));

		}
		else {

			// Record the insert
			RecordingQueue.addToQueue(
					ActionFactory.createItemStack("item-insert", hand, 1, 0, null, block.getLocation(), player));

		}
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.HIGHEST)
	public void onPlayerEntityInteract(final PlayerInteractEntityEvent event) {

		final Player player = event.getPlayer();
		final Entity entity = event.getRightClicked();

		// Are they using a wand?
		if (Prism.playersWithActiveTools.containsKey(player.getName())) {

			// Pull the wand in use
			final Wand wand = Prism.playersWithActiveTools.get(player.getName());
			if (wand != null && wand instanceof ProfileWand) {

				wand.playerRightClick(player, entity);

				// Always cancel
				event.setCancelled(true);

			}
		}
	}
}
