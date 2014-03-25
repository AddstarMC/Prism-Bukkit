package me.botsko.prism.actions;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.Location;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.Hanging;
import org.bukkit.entity.ItemFrame;
import org.bukkit.entity.Painting;
import org.bukkit.entity.Player;

public class HangingItemAction extends GenericAction {

    public class HangingItemActionData {
        public String type;
        public String direction;
    }

    /**
	 * 
	 */
    protected HangingItemActionData actionData;

    /**
     * 
     * @param hanging
     */
    public void setHanging(Hanging hanging) {

        actionData = new HangingItemActionData();

        if( hanging != null ) {
            this.actionData.type = hanging.getType().name().toLowerCase();
            this.actionData.direction = hanging.getAttachedFace().name().toLowerCase();
            this.world_name = hanging.getWorld().getName();
            this.x = hanging.getLocation().getBlockX();
            this.y = hanging.getLocation().getBlockY();
            this.z = hanging.getLocation().getBlockZ();
        }
    }

    /**
	 * 
	 */
    @Override
    public void setData(String data) {
        this.data = data;
        if( data != null && data.startsWith( "{" ) ) {
            actionData = gson.fromJson( data, HangingItemActionData.class );
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
     * @return
     */
    public String getHangingType() {
        return this.actionData.type;
    }

    /**
     * 
     * @return
     */
    public BlockFace getDirection() {
        if( actionData.direction != null ) { return BlockFace.valueOf( actionData.direction.toUpperCase() ); }
        return null;
    }

    /**
     * 
     * @return
     */
    @Override
    public String getNiceName() {
        return this.actionData.type != null ? this.actionData.type : data.toLowerCase();
    }

    /**
	 * 
	 */
    @Override
    public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview) {
        return hangItem( player, parameters, is_preview );
    }

    /**
	 * 
	 */
    @Override
    public ChangeResult applyRestore(Player player, QueryParameters parameters, boolean is_preview) {
        return hangItem( player, parameters, is_preview );
    }

    /**
	 * 
	 */
    public ChangeResult hangItem(Player player, QueryParameters parameters, boolean is_preview) {

        final BlockFace attachedFace = getDirection();

        final Location loc = new Location( getWorld(), getX(), getY(), getZ() ).getBlock().getRelative( getDirection() )
                .getLocation();

        // Ensure there's a block at this location that accepts an attachment
        if( me.botsko.elixr.BlockUtils.materialMeansBlockDetachment( loc.getBlock().getType() ) ) { return new ChangeResult(
                ChangeResultType.SKIPPED, null ); }

        try {
            if( getHangingType().equals( "item_frame" ) ) {
                final Hanging hangingItem = getWorld().spawn( loc, ItemFrame.class );
                hangingItem.setFacingDirection( attachedFace, true );
                return new ChangeResult( ChangeResultType.APPLIED, null );
            } else if( getHangingType().equals( "painting" ) ) {
                final Hanging hangingItem = getWorld().spawn( loc, Painting.class );
                hangingItem.setFacingDirection( getDirection(), true );
                return new ChangeResult( ChangeResultType.APPLIED, null );
            }
        } catch ( final IllegalArgumentException e ) {
            // Something interfered with being able to place the painting
        }
        return new ChangeResult( ChangeResultType.SKIPPED, null );
    }
}