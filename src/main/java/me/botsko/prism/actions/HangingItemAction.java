package me.botsko.prism.actions;

import com.helion3.prism.libs.elixr.BlockUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.appliers.PrismProcessType;

import org.bukkit.Art;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.Rotation;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.*;

public class HangingItemAction extends GenericAction {

    public class HangingItemActionData {
        public String type;
        public String direction;
        public String rotation;
        public String art;
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
            if(hanging.getAttachedFace() != null) {
                this.actionData.direction = hanging.getAttachedFace().name().toLowerCase();
            }
            this.world_name = hanging.getWorld().getName();
            this.x = hanging.getLocation().getBlockX();
            this.y = hanging.getLocation().getBlockY();
            this.z = hanging.getLocation().getBlockZ();

            if (hanging.getType().equals(EntityType.PAINTING)) {
                final Painting painting = (Painting) hanging;
                this.actionData.art = painting.getArt().name().toLowerCase();

                // Sometimes coordinates of the painting are not equal placement coordinates. Fix it.
                final double offsetY = -0.5;
                double offsetX = 0;
                double offsetZ = 0;
                if (painting.getFacing().equals(BlockFace.SOUTH)) {
                    offsetX = -0.5;
                } else if (painting.getFacing().equals(BlockFace.WEST)) {
                    offsetZ = -0.5;
                }
                final Location addLoc = painting.getLocation().add(offsetX, offsetY, offsetZ);
                this.x = addLoc.getBlockX();
                this.y = addLoc.getBlockY();
                this.z = addLoc.getBlockZ();
            } else if (hanging.getType().equals(EntityType.ITEM_FRAME)) {
                final ItemFrame frame = (ItemFrame) hanging;
                final Rotation rotation = frame.getRotation();

                if ( getType().getName().equals("item-rotate") ) {
                    // Don't save type for rotation events; it'll always be item frame
                    this.actionData.type = null;
                    // Use new rotation instead of current (old) one
                    this.actionData.rotation = rotation.rotateClockwise().name().toLowerCase();
                } else {
                    this.actionData.rotation = rotation.name().toLowerCase();
                }
            }
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
    public Rotation getRotation() {
        if( actionData.rotation != null ) { return Rotation.valueOf( actionData.rotation.toUpperCase() ); }
        return Rotation.NONE;
    }

    /**
     * 
     * @return
     */
    public Art getArt() {
        if( actionData.art != null ) { return Art.valueOf( actionData.art.toUpperCase() ); }
        return null;
    }

    /**
     * 
     * @return
     */
    @Override
    public String getNiceName() {
        if ( getType().getName().equals("item-rotate") ) {
            // Special case for older item-rotate records
            String rotation = this.actionData != null ? this.actionData.rotation : data.toLowerCase();

            switch (rotation) {
                case "none": return "0°";
                case "clockwise_45": return "45°";
                case "clockwise": return "90°";
                case "clockwise_135": return "135°";
                case "flipped": return "180°";
                case "flipped_45": return "225°";
                case "counter_clockwise": return "270°";
                case "counter_clockwise_45": return "315°";
                default: return rotation;
            }
        } else {
            return this.actionData.type != null ? this.actionData.type : data.toLowerCase();
        }
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

        final BlockFace facingDirection = getDirection().getOppositeFace();

        final Location loc = new Location( getWorld(), getX(), getY(), getZ() );
        final Location locAcceptor = loc.getBlock().getRelative(facingDirection.getOppositeFace()).getLocation();

        // Ensure there's a block at this location that accepts an attachment
        if( BlockUtils.materialMeansBlockDetachment( locAcceptor.getBlock().getType() ) ) {
            return new ChangeResult(ChangeResultType.SKIPPED, null );
        }

        if ((getType().getName().equals("hangingitem-break") && parameters.getProcessType().equals(PrismProcessType.ROLLBACK)) ||
            (getType().getName().equals("hangingitem-place") && parameters.getProcessType().equals(PrismProcessType.RESTORE))) {

            // We should place the ItemFrame or Painting
            try {
                if( getHangingType().equals( "item_frame" ) ) {
                    final ItemFrame frame = getWorld().spawn( loc, ItemFrame.class );
                    frame.teleport(loc);
                    frame.setFacingDirection( facingDirection, true );
                    frame.setRotation( getRotation() );
                    return new ChangeResult( ChangeResultType.APPLIED, null );
                } else if( getHangingType().equals( "painting" ) ) {
                    final Hanging hangingItem = getWorld().spawn( loc, Painting.class );
                    ((Painting)hangingItem).setArt(getArt(), true);
                    hangingItem.teleport(loc);
                    hangingItem.setFacingDirection( facingDirection, true );
                    return new ChangeResult( ChangeResultType.APPLIED, null );
                }
            } catch ( final IllegalArgumentException e ) {
                // Something interfered with being able to place the painting
                Prism.debug("Could not place item frame: " + e.getMessage());
                Prism.debug(locAcceptor);
            }

        } else if ((getType().getName().equals("hangingitem-place") && parameters.getProcessType().equals(PrismProcessType.ROLLBACK)) ||
                   (getType().getName().equals("hangingitem-break") && parameters.getProcessType().equals(PrismProcessType.RESTORE))) {

            // We should remove the ItemFrame or Painting
            for ( Hanging e : BlockUtils.findHangingEntities(loc) ) {
                if (e.getType().equals(EntityType.ITEM_FRAME)) {
                    final ItemFrame frame = (ItemFrame) e;
                    if (frame.getFacing().equals(facingDirection) && frame.getItem().getType().equals(Material.AIR)) {
                        frame.remove();
                        return new ChangeResult( ChangeResultType.APPLIED, null );
                    }
                } else if (e.getType().equals(EntityType.PAINTING)) {
                    final Painting painting = (Painting) e;
                    if (painting.getFacing().equals(facingDirection) && painting.getArt().equals(getArt())) {
                        painting.remove();
                        return new ChangeResult( ChangeResultType.APPLIED, null );
                    }
                }
            }

        } else if ( getType().getName().equals("item-rotate") ) {
            for ( Hanging e : BlockUtils.findHangingEntities(loc) ) {
                if (!e.getType().equals(EntityType.ITEM_FRAME)) continue;

                final ItemFrame frame = (ItemFrame) e;
                if (!frame.getFacing().equals(facingDirection)) continue;

                if (parameters.getProcessType().equals(PrismProcessType.ROLLBACK)) {
                    frame.setRotation( getRotation().rotateCounterClockwise() );
                    return new ChangeResult( ChangeResultType.APPLIED, null );
                } else if (parameters.getProcessType().equals(PrismProcessType.RESTORE)) {
                    frame.setRotation( getRotation().rotateClockwise() );
                    return new ChangeResult( ChangeResultType.APPLIED, null );
                }
            }
        }

        return new ChangeResult( ChangeResultType.SKIPPED, null );
    }
}