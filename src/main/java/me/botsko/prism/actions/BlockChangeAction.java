package me.botsko.prism.actions;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.utils.block.Utilities;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.data.BlockData;
import org.bukkit.entity.Player;

public class BlockChangeAction extends BlockAction {

    @Override
    public String getNiceName() {
        String name;
        if (this.getActionType().getName().equals("block-fade")) {
            name = Prism.getItems().getAlias(getOldMaterial(), getOldBlockData());
        } else {
            name = super.getNiceName();
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
        if (parameters.getProcessType().equals(PrismProcessType.ROLLBACK)) {
            // Run verification for no-overwrite. Only reverse a change
            // if the opposite state is what's present now.
            // We skip this check because if we're in preview mode the block may
            // not have been properly changed yet.
            return processChange(player, parameters, isPreview, newMat, oldMat, oldData, block, isDeferred, b);
        }
        if (parameters.getProcessType().equals(PrismProcessType.RESTORE)) {
            return processChange(player, parameters, isPreview, oldMat, newMat, newData, block, isDeferred, b);
        }
        if (parameters.getProcessType().equals(PrismProcessType.UNDO)) {
            b.setMaterial(oldMat);
            b.setBlockData(oldData);
            return b.placeBlock(player, parameters, isPreview, block, isDeferred);
        }
        return new ChangeResult(ChangeResultType.SKIPPED, null);
    }

    private ChangeResult processChange(Player player, QueryParameters parameters, boolean isPreview,
                                       Material originalMaterial, Material replacedMaterial, BlockData replacedData,
                                       Block currentBlock, boolean isDeferred, BlockAction action) {
        if (Utilities.isAcceptableForBlockPlace(currentBlock.getType())
                || Utilities.areBlockIdsSameCoreItem(currentBlock.getType(), originalMaterial) || isPreview
                || parameters.hasFlag(Flag.OVERWRITE)) {
            action.setMaterial(replacedMaterial);
            action.setBlockData(replacedData);
            return action.placeBlock(player, parameters, isPreview, currentBlock, isDeferred);
        } else {
            Prism.debug("Skipped Change for " + parameters.getProcessType().name() + " because current-> "
                    + currentBlock.getType() + " != " + originalMaterial.name() + " <- what we think we will replace.");
            return new ChangeResult(ChangeResultType.SKIPPED, null);
        }
    }
}



