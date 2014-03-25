package me.botsko.prism.actions;

import org.bukkit.Location;
import org.bukkit.block.Block;

public class BlockShiftAction extends GenericAction {

    public class BlockShiftActionData {
        public int x;
        public int y;
        public int z;
    }

    /**
	 * 
	 */
    protected BlockShiftActionData actionData;

    /**
     * 
     * @param from
     */
    public void setBlock(Block from) {

        // Build an object for the specific details of this action
        actionData = new BlockShiftActionData();

        // Store information for the action
        if( from != null ) {
            this.block_id = from.getTypeId();
            this.block_subid = from.getData();
            actionData.x = from.getX();
            actionData.y = from.getY();
            actionData.z = from.getZ();
            this.world_name = from.getWorld().getName();

        }
    }

    /**
     * 
     * @param to
     */
    public void setToLocation(Location to) {
        if( to != null ) {
            this.x = to.getBlockX();
            this.y = to.getBlockY();
            this.z = to.getBlockZ();
        }
    }

    /**
	 * 
	 */
    @Override
    public void save() {
        data = gson.toJson( actionData );
    }

    /**
	 * 
	 */
    @Override
    public void setData(String data) {
        this.data = data;
        if( data != null && data.startsWith( "{" ) ) {
            actionData = gson.fromJson( data, BlockShiftActionData.class );
        }
    }

    /**
     * 
     * @return
     */
    @Override
    public String getNiceName() {
        return this.materialAliases.getAlias( this.block_id, this.block_subid ) + " from " + actionData.x + " "
                + actionData.z;
    }
}