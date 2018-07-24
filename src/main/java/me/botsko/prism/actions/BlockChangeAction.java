package me.botsko.prism.actions;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.data.BlockData;
import org.bukkit.entity.Player;

public class BlockChangeAction extends BlockAction {

	/**
	 * 
	 * @return
	 */
	@Override
	public String getNiceName() {
		String name = "";
		if (this.getType().getName().equals("block-fade")) {
			name += materialAliases.getAlias(this.old_block, this.old_block_data);
		}
		else {
			name += materialAliases.getAlias(this.block, this.block_data);
		}
		return name;
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview) {
		final Block block = getWorld().getBlockAt(getLoc());
		return placeBlock(player, parameters, is_preview, getType().getName(), getOldBlock(), getOldBlockData(),
				getBlock(), getBlockData(), block, false);
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyRestore(Player player, QueryParameters parameters, boolean is_preview) {
		final Block block = getWorld().getBlockAt(getLoc());
		return placeBlock(player, parameters, is_preview, getType().getName(), getOldBlock(), getOldBlockData(),
				getBlock(), getBlockData(), block, false);
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyUndo(Player player, QueryParameters parameters, boolean is_preview) {
		final Block block = getWorld().getBlockAt(getLoc());
		return placeBlock(player, parameters, is_preview, getType().getName(), getOldBlock(), getOldBlockData(),
				getBlock(), getBlockData(), block, false);
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyDeferred(Player player, QueryParameters parameters, boolean is_preview) {
		final Block block = getWorld().getBlockAt(getLoc());
		return placeBlock(player, parameters, is_preview, getType().getName(), getOldBlock(), getOldBlockData(),
				getBlock(), getBlockData(), block, true);
	}

	/**
	 * 
	 * @param type
	 * @param old_id
	 * @param old_subid
	 * @param new_id
	 * @param new_subid
	 * @param block
	 * @param is_deferred
	 * @return
	 */
	protected ChangeResult placeBlock(Player player, QueryParameters parameters, boolean is_preview, String type,
			Material old_mat, BlockData old_data, Material new_mat, BlockData new_data, Block block,
			boolean is_deferred) {

		final BlockAction b = new BlockAction();
		b.setActionType(type);
		b.setPlugin(plugin);
		b.setWorldName(getWorldName());
		b.setX(getX());
		b.setY(getY());
		b.setZ(getZ());
		if (parameters.getProcessType().equals(PrismProcessType.ROLLBACK)) {
			// Run verification for no-overwrite. Only reverse a change
			// if the opposite state is what's present now.
			// We skip this check because if we're in preview mode the block may
			// not
			// have been properly changed yet.
			// https://snowy-evening.com/botsko/prism/302/
			// and https://snowy-evening.com/botsko/prism/258/
			if (BlockUtils.isAcceptableForBlockPlace(block.getType())
					|| BlockUtils.areBlockIdsSameCoreItem(block.getType(), new_mat) || is_preview
					|| parameters.hasFlag(Flag.OVERWRITE)) {
				b.setBlock(old_mat);
				b.setBlockData(old_data);
				return b.placeBlock(player, parameters, is_preview, block, is_deferred);
			}
			else {
				// System.out.print("Block change skipped because new id doesn't match what's
				// there now. There now: "
				// + block.getTypeId() + " vs " + new_id);
				return new ChangeResult(ChangeResultType.SKIPPED, null);
			}
		}
		else if (parameters.getProcessType().equals(PrismProcessType.RESTORE)) {
			// Run verification for no-overwrite. Only reapply a change
			// if the opposite state is what's present now.
			// We skip this check because if we're in preview mode the block may
			// not
			// have been properly changed yet.
			// https://snowy-evening.com/botsko/prism/302/
			// and https://snowy-evening.com/botsko/prism/258/
			if (BlockUtils.isAcceptableForBlockPlace(block.getType())
					|| BlockUtils.areBlockIdsSameCoreItem(block.getType(), old_mat) || is_preview
					|| parameters.hasFlag(Flag.OVERWRITE)) {
				b.setBlock(new_mat);
				b.setBlockData(new_data);
				return b.placeBlock(player, parameters, is_preview, block, is_deferred);
			}
			else {
				// System.out.print("Block change skipped because old id doesn't match what's
				// there now. There now: "
				// + block.getTypeId() + " vs " + old_id);
				return new ChangeResult(ChangeResultType.SKIPPED, null);
			}
		}
		if (parameters.getProcessType().equals(PrismProcessType.UNDO)) {
			b.setBlock(old_mat);
			b.setBlockData(old_data);
			return b.placeBlock(player, parameters, is_preview, block, is_deferred);
		}
		return new ChangeResult(ChangeResultType.SKIPPED, null);
	}
}