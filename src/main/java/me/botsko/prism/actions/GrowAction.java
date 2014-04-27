package me.botsko.prism.actions;

import org.bukkit.block.BlockState;

public class GrowAction extends BlockAction {

    /**
     * 
     * @param state
     */
    public void setBlock(BlockState state) {
        if( state != null ) {
            this.block_id = state.getTypeId();
            this.block_subid = state.getData().getData();
            this.world_name = state.getWorld().getName();
            this.x = state.getLocation().getBlockX();
            this.y = state.getLocation().getBlockY();
            this.z = state.getLocation().getBlockZ();
        }
    }
}