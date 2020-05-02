package me.botsko.prism.actions;

import me.botsko.prism.Prism;
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

    @Override
    public String getNiceName() {
        String name = "";
        if (this.getActionType().getName().equals("block-fade")) {
            name += Prism.getItems().getAlias(getOldMaterial(), getOldBlockData());
        } else {
            name += Prism.getItems().getAlias(getMaterial(), getBlockData());
        }
        return name;
    }

    @Override
    public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean isPreview) {
        final Block block = getWorld().getBlockAt(getLoc());
        return placeBlock(player, parameters, isPreview, getActionType().getName(), getOldMaterial(), getOldBlockData(),
                getMaterial(), getBlockData(), block, false);
    }

    @Override
    public ChangeResult applyRestore(Player player, QueryParameters parameters, boolean isPreview) {
        final Block block = getWorld().getBlockAt(getLoc());
        return placeBlock(player, parameters, isPreview, getActionType().getName(), getOldMaterial(), getOldBlockData(),
                getMaterial(), getBlockData(), block, false);
    }

    @Override
    public ChangeResult applyUndo(Player player, QueryParameters parameters, boolean isPreview) {
        final Block block = getWorld().getBlockAt(getLoc());
        return placeBlock(player, parameters, isPreview, getActionType().getName(), getOldMaterial(), getOldBlockData(),
                getMaterial(), getBlockData(), block, false);
    }

    @Override
    public ChangeResult applyDeferred(Player player, QueryParameters parameters, boolean isPreview) {
        final Block block = getWorld().getBlockAt(getLoc());
        return placeBlock(player, parameters, isPreview, getActionType().getName(), getOldMaterial(), getOldBlockData(),
                getMaterial(), getBlockData(), block, true);
    }

    private ChangeResult placeBlock(Player player, QueryParameters parameters, boolean isPreview, String type,
                                    Material oldMat, BlockData oldData, Material newMat, BlockData newData, Block block,
                                    boolean isDeferred) {

        final BlockAction b = new BlockAction();
        b.setActionType(type);
        b.setLoc(getLoc());
        if (parameters.getProcessType().equals(PrismProcessType.ROLLBACK)
                || parameters.getProcessType().equals(PrismProcessType.RESTORE)) {
            // Run verification for no-overwrite. Only reverse a change
            // if the opposite state is what's present now.
            // We skip this check because if we're in preview mode the block may
            // not
            // have been properly changed yet.
            // https://snowy-evening.com/botsko/prism/302/
            // and https://snowy-evening.com/botsko/prism/258/
            return checkCanPlaceBlockType(block, b, player, parameters, oldMat, newMat, newData, isPreview, isDeferred);
        }
        if (parameters.getProcessType().equals(PrismProcessType.UNDO)) {
            b.setMaterial(oldMat);
            b.setBlockData(oldData);
            return b.placeBlock(player, parameters, isPreview, block, isDeferred);
        }
        return new ChangeResult(ChangeResultType.SKIPPED, null);
    }

    private ChangeResult checkCanPlaceBlockType(Block block, BlockAction b, Player player,
                                                QueryParameters parameters, Material oldMat, Material newMat,
                                                BlockData newData, boolean isPreview, boolean isDeferred) {
        if (BlockUtils.isAcceptableForBlockPlace(block.getType())
                || BlockUtils.areBlockIdsSameCoreItem(block.getType(), oldMat) || isPreview
                || parameters.hasFlag(Flag.OVERWRITE)) {
            b.setMaterial(newMat);
            b.setBlockData(newData);
            return b.placeBlock(player, parameters, isPreview, block, isDeferred);
        } else {
            // System.out.print("Block change skipped because old id doesn't match what's
            // there now. There now: "
            // + block.getTypeId() + " vs " + old_id);
            return new ChangeResult(ChangeResultType.SKIPPED, null);
        }
    }
}



