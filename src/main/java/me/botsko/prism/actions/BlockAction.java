package me.botsko.prism.actions;

import me.botsko.prism.utils.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.EntityUtils;
import me.botsko.prism.utils.MaterialTag;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.block.CommandBlock;
import org.bukkit.block.CreatureSpawner;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;
import org.bukkit.block.data.Bisected;
import org.bukkit.block.data.BlockData;
import org.bukkit.block.data.Directional;
import org.bukkit.block.data.Waterlogged;
import org.bukkit.block.data.Bisected.Half;
import org.bukkit.block.data.type.Bed;
import org.bukkit.block.data.type.Bed.Part;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;

public class BlockAction extends GenericAction {

	/**
	 * 
	 */
	protected BlockActionData actionData;

	/**
	 *
	 * @param block
	 */
	public void setBlock(Block block) {
		if (block != null) {
			setBlock(block.getState());
		}
	}

	/**
	 *
	 * @param state
	 */
	public void setBlock(BlockState state) {
		if (state != null) {

			block = state.getType();
			block_data = state.getBlockData();

			// spawner
			if (state.getType() == Material.SPAWNER) {
				final SpawnerActionData spawnerActionData = new SpawnerActionData();
				final CreatureSpawner s = (CreatureSpawner) state;
				spawnerActionData.entity_type = s.getSpawnedType().name().toLowerCase();
				spawnerActionData.delay = s.getDelay();
				actionData = spawnerActionData;
			}

			// skulls
			else if (state instanceof Skull) {
				final SkullActionData skullActionData = new SkullActionData();
				final Skull s = (Skull) state;
				final Directional d = (Directional) state.getBlockData();
				skullActionData.rotation = d.getFacing().name().toLowerCase();

				if (state.getType() == Material.PLAYER_HEAD || state.getType() == Material.PLAYER_WALL_HEAD) {
					skullActionData.owner = s.getOwningPlayer().getUniqueId().toString();
				}

				actionData = skullActionData;
			}

			// signs
			else if ((state.getType() == Material.SIGN || state.getType() == Material.WALL_SIGN)) {
				final SignActionData signActionData = new SignActionData();
				final Sign s = (Sign) state;
				signActionData.lines = s.getLines();
				actionData = signActionData;
			}

			// command block
			else if ((state.getType() == Material.COMMAND_BLOCK)) {
				final CommandBlock cmdblock = (CommandBlock) state;
				data = cmdblock.getCommand();
			}

			this.world_name = state.getWorld().getName();
			this.x = state.getLocation().getBlockX();
			this.y = state.getLocation().getBlockY();
			this.z = state.getLocation().getBlockZ();
		}
	}

	/**
	 * 
	 */
	@Override
	public void setData(String data) {
		this.data = data;
		if (data != null && data.startsWith("{")) {
			if (block == Material.PLAYER_HEAD || block == Material.PLAYER_WALL_HEAD) {
				actionData = gson.fromJson(data, SkullActionData.class);
			}
			else if (block == Material.SPAWNER) {
				actionData = gson.fromJson(data, SpawnerActionData.class);
			}
			else if (block == Material.SIGN || block == Material.WALL_SIGN) {
				actionData = gson.fromJson(data, SignActionData.class);
			}
			else if (block == Material.COMMAND_BLOCK) {
				actionData = new BlockActionData();
			}
			else {
				// No longer used, was for pre-1.5 data formats
			}
		}
	}

	/**
	 * 
	 */
	@Override
	public void save() {
		// Only for the blocks we store meta data for
		if (actionData != null) {
			data = gson.toJson(actionData);
		}
	}

	/**
	 * 
	 * @return
	 */
	public BlockActionData getActionData() {
		return actionData;
	}

	/**
	 * 
	 * @return
	 */
	@Override
	public String getNiceName() {
		String name = "";
		BlockActionData blockActionData = getActionData();

		if (blockActionData instanceof SkullActionData) {
			final SkullActionData ad = (SkullActionData) blockActionData;
			name += ad.skull_type + " ";
		}
		else if (blockActionData instanceof SpawnerActionData) {
			final SpawnerActionData ad = (SpawnerActionData) blockActionData;
			name += ad.entity_type + " ";
		}
		name += materialAliases.getAlias(this.block, this.block_data);
		if (blockActionData instanceof SignActionData) {
			final SignActionData ad = (SignActionData) blockActionData;
			if (ad.lines != null && ad.lines.length > 0) {
				name += " (" + TypeUtils.join(ad.lines, ", ") + ")";
			}
		}
		else if (block == Material.COMMAND_BLOCK) {
			name += " (" + data + ")";
		}
		if (type.getName().equals("crop-trample") && block == Material.AIR) {
			return "empty soil";
		}
		return name;
	}

	@Override
	public String getCustomDesc() {
		if (type.getName().equals("water-bucket") && getBlockData() instanceof Waterlogged) {
			return "waterlogged";
		}

		return null;
	}

	/**
	 * 
	 * @author botskonet
	 * 
	 */
	public class BlockActionData {
	}

	/**
	 * 
	 * @author botskonet
	 */
	public class SpawnerActionData extends BlockActionData {

		public String entity_type;
		public int delay;

		/**
		 * 
		 * @return
		 */
		public EntityType getEntityType() {
			return EntityType.valueOf(entity_type.toUpperCase());
		}

		/**
		 * 
		 * @return
		 */
		public int getDelay() {
			return delay;
		}
	}

	/**
	 * 
	 * @author botskonet
	 */
	public class SkullActionData extends BlockActionData {

		public String rotation;
		public String owner;
		public String skull_type;

		/**
		 * 
		 * @return
		 */
		public BlockFace getRotation() {
			if (rotation != null) {
				return BlockFace.valueOf(rotation.toUpperCase());
			}
			return null;
		}
	}

	/**
	 * Not to be confused with SignChangeActionData, which records additional data
	 * we don't need here.
	 * 
	 * @author botskonet
	 * 
	 */
	public class SignActionData extends BlockActionData {
		public String[] lines;
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview) {
		final Block block = getWorld().getBlockAt(getLoc());
		if (getType().doesCreateBlock()) {
			return removeBlock(player, parameters, is_preview, block);
		}
		else {
			return placeBlock(player, parameters, is_preview, block, false);
		}
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyRestore(Player player, QueryParameters parameters, boolean is_preview) {
		final Block block = getWorld().getBlockAt(getLoc());
		if (getType().doesCreateBlock()) {
			return placeBlock(player, parameters, is_preview, block, false);
		}
		else {
			return removeBlock(player, parameters, is_preview, block);
		}
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyUndo(Player player, QueryParameters parameters, boolean is_preview) {

		final Block block = getWorld().getBlockAt(getLoc());

		// Undo a drain/ext event (which always remove blocks)
		// @todo if we ever track rollback/restore for undo, we'll
		// need logic to do the opposite
		return placeBlock(player, parameters, is_preview, block, false);

	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyDeferred(Player player, QueryParameters parameters, boolean is_preview) {
		final Block block = getWorld().getBlockAt(getLoc());
		return placeBlock(player, parameters, is_preview, block, true);
	}

	/**
	 * Place a block unless something other than air occupies the spot, or if we
	 * detect a falling block now sits there. This resolves the issue of falling
	 * blocks taking up the space, preventing this rollback. However, it also means
	 * that a rollback *could* interfere with a player-placed block.
	 */
	protected ChangeResult placeBlock(Player player, QueryParameters parameters, boolean is_preview, Block block,
			boolean is_deferred) {

		BlockStateChange stateChange;

		BlockState state = block.getState();

		// Ensure block action is allowed to place a block here.
		// (essentially liquid/air).

		final boolean cancelIfBadPlace = !getType().requiresHandler("BlockChangeAction")
				&& !getType().requiresHandler("PrismRollbackAction") && !parameters.hasFlag(Flag.OVERWRITE);

		if (cancelIfBadPlace && !BlockUtils.isAcceptableForBlockPlace(block.getType())) {
			// System.out.print("Block skipped due to being unaccaptable for block place.");
			return new ChangeResult(ChangeResultType.SKIPPED, null);
		}

		// On the blacklist (except an undo)
		if (Prism.getIllegalBlocks().contains(getBlock())
				&& !parameters.getProcessType().equals(PrismProcessType.UNDO)) {
			// System.out.print("Block skipped because it's not allowed to be placed.");
			return new ChangeResult(ChangeResultType.SKIPPED, null);
		}

		// If we're not in a preview, actually apply this block
		if (!is_preview) {

			// Capture the block before we change it
			final BlockState originalBlock = block.getState();

			// If lilypad, check that block below is water. Be sure
			// it's set to stationary water so the lilypad will sit
			if (getBlock() == Material.LILY_PAD) {

				final Block below = block.getRelative(BlockFace.DOWN);
				if (below.getType().equals(Material.WATER) || below.getType().equals(Material.AIR)) {
					below.setType(Material.WATER);
				}
				else {
					// Prism.debug("Lilypad skipped because no water exists below.");
					return new ChangeResult(ChangeResultType.SKIPPED, null);
				}
			}

			// If portal, we need to light the portal. seems to be the only way.
			if (getBlock() == Material.NETHER_PORTAL) {
				final Block obsidian = BlockUtils.getFirstBlockOfMaterialBelow(Material.OBSIDIAN, block.getLocation());
				if (obsidian != null) {
					final Block above = obsidian.getRelative(BlockFace.UP);
					if (!(above.getType() == Material.NETHER_PORTAL)) {
						above.setType(Material.FIRE);
						return new ChangeResult(ChangeResultType.APPLIED, null);
					}
				}
			}

			// Jukebox, never use the data val because
			// it becomes unplayable
			if (getBlock() == Material.JUKEBOX) {
				block_data = Bukkit.createBlockData(Material.JUKEBOX);
			}

			// Set the material
			state.setType(getBlock());

			BlockActionData blockActionData = getActionData();

			/**
			 * Skulls
			 */
			if ((getBlock() == Material.PLAYER_HEAD || getBlock() == Material.PLAYER_WALL_HEAD)
					&& blockActionData instanceof SkullActionData) {

				final SkullActionData s = (SkullActionData) blockActionData;

				// Set skull data
				final Directional direction = (Directional) state.getBlockData();
				direction.setFacing(s.getRotation());

				if (!s.owner.isEmpty()) {
					final Skull skull = (Skull) state;
					skull.setOwningPlayer(Bukkit.getOfflinePlayer(EntityUtils.uuidOf((s.owner))));
				}

			}

			/**
			 * Spawner
			 */
			if (getBlock() == Material.SPAWNER && blockActionData instanceof SpawnerActionData) {

				final SpawnerActionData s = (SpawnerActionData) blockActionData;

				// Set spawner data
				final CreatureSpawner spawner = (CreatureSpawner) state;
				spawner.setDelay(s.getDelay());
				spawner.setSpawnedType(s.getEntityType());

			}

			/**
			 * Restoring command block
			 */
			if (getBlock() == Material.COMMAND_BLOCK) {
				final CommandBlock cmdblock = (CommandBlock) state;
				cmdblock.setCommand(data);
			}

			/**
			 * Signs
			 */
			if (parameters.getProcessType() == PrismProcessType.ROLLBACK
					&& (getBlock() == Material.SIGN || getBlock() == Material.WALL_SIGN)
					&& blockActionData instanceof SignActionData) {

				final SignActionData s = (SignActionData) blockActionData;

				// Verify block is sign. Rarely, if the block somehow pops off
				// or fails
				// to set it causes ClassCastException:
				// org.bukkit.craftbukkit.v1_4_R1.block.CraftBlockState
				// cannot be cast to org.bukkit.block.Sign
				// https://snowy-evening.com/botsko/prism/455/
				if (state instanceof Sign) {

					// Set sign data
					final Sign sign = (Sign) state;

					if (s.lines != null) {
						for (int i = 0; i < s.lines.length; ++i) {
							sign.setLine(i, s.lines[i]);
						}
					}
				}
			}

			state.setBlockData(getBlockData());

			// -----------------------------
			// Sibling logic marker

			// If the material is a crop that needs soil, we must restore the
			// soil
			// This may need to go before setting the block, but I prefer the
			// BlockUtil
			// logic to use materials.
			BlockState sibling = null;

			if (BlockUtils.materialRequiresSoil(getBlock())) {
				sibling = block.getRelative(BlockFace.DOWN).getState();

				if (cancelIfBadPlace && !MaterialTag.SOIL_CANDIDATES.isTagged(sibling.getType())) {
					return new ChangeResult(ChangeResultType.SKIPPED, null);
				}

				sibling.setType(Material.FARMLAND);
			}

			// Chest sides can be broken independently, ignore them
			if (state.getType() != Material.CHEST && state.getType() != Material.TRAPPED_CHEST) {
				final Block s = BlockUtils.getSiblingForDoubleLengthBlock(state);

				if (s != null) {
					sibling = s.getState();

					if (cancelIfBadPlace && !BlockUtils.isAcceptableForBlockPlace(sibling.getType())) {
						// Upper half fail
						return new ChangeResult(ChangeResultType.SKIPPED, null);
					}

					sibling.setType(block.getType());

					BlockData siblingData = getBlockData().clone();

					if (siblingData instanceof Bed) {
						// We always log the foot
						((Bed) siblingData).setPart(Part.HEAD);
					}
					else if (siblingData instanceof Bisected) {
						// We always log the bottom
						((Bisected) siblingData).setHalf(Half.TOP);
					}

					sibling.setBlockData(siblingData);
				}
			}

			state.update(true, false);

			if (sibling != null) {
				sibling.update(true, false);
			}

			// Store the state change
			stateChange = new BlockStateChange(originalBlock, state);
		}
		else {

			// Otherwise, save the state so we can cancel if needed
			final BlockState originalBlock = block.getState();
			// Note: we save the original state as both old/new so we can re-use
			// blockStateChanges
			stateChange = new BlockStateChange(originalBlock, originalBlock);

			// Preview it
			EntityUtils.sendBlockChange(player, block.getLocation(), getBlockData());

			// Send preview to shared players
			for (final CommandSender sharedPlayer : parameters.getSharedPlayers()) {
				if (sharedPlayer instanceof Player) {
					EntityUtils.sendBlockChange((Player) sharedPlayer, block.getLocation(), getBlockData());
				}
			}
		}

		return new ChangeResult(ChangeResultType.APPLIED, stateChange);

	}

	/**
	 * 
	 */
	protected ChangeResult removeBlock(Player player, QueryParameters parameters, boolean is_preview, Block block) {

		BlockStateChange stateChange;

		if (!block.getType().equals(Material.AIR)) {

			// Ensure it's acceptable to remove the current block
			if (!BlockUtils.isAcceptableForBlockPlace(block.getType())
					&& !BlockUtils.areBlockIdsSameCoreItem(block.getType(), getBlock())
					&& !parameters.hasFlag(Flag.OVERWRITE)) {
				return new ChangeResult(ChangeResultType.SKIPPED, null);
			}

			if (!is_preview) {

				// Capture the block before we change it
				final BlockState originalBlock = block.getState();

				// Set
				block.setType(Material.AIR);

				// Capture the new state
				final BlockState newBlock = block.getState();

				// Store the state change
				stateChange = new BlockStateChange(originalBlock, newBlock);

			}
			else {

				// Otherwise, save the state so we can cancel if needed
				final BlockState originalBlock = block.getState();
				// Note: we save the original state as both old/new so we can
				// re-use blockStateChanges
				stateChange = new BlockStateChange(originalBlock, originalBlock);

				// Preview it
				EntityUtils.sendBlockChange(player, block.getLocation(), Bukkit.createBlockData(Material.AIR));

				// Send preview to shared players
				for (final CommandSender sharedPlayer : parameters.getSharedPlayers()) {
					if (sharedPlayer instanceof Player) {
						EntityUtils.sendBlockChange((Player) sharedPlayer, block.getLocation(),
								Bukkit.createBlockData(Material.AIR));
					}
				}
			}
			return new ChangeResult(ChangeResultType.APPLIED, stateChange);
		}
		return new ChangeResult(ChangeResultType.SKIPPED, null);
	}
}