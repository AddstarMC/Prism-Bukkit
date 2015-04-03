package me.botsko.prism.actions;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
<<<<<<< HEAD
=======
import me.botsko.prism.appliers.PrismProcessType;

import org.bukkit.Art;
>>>>>>> bukit1710
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.BlockFace;
<<<<<<< HEAD
import org.bukkit.entity.*;
=======
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Hanging;
import org.bukkit.entity.ItemFrame;
import org.bukkit.entity.Painting;
import org.bukkit.entity.Player;
>>>>>>> bukit1710

public class HangingItemAction extends GenericAction {

    public class HangingItemActionData {
        public String type;
        public String direction;
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

        final BlockFace facingDirection = getDirection();

        final Location loc = new Location( getWorld(), getX(), getY(), getZ() );
        final Location locAcceptor = loc.getBlock().getRelative(facingDirection.getOppositeFace()).getLocation();

        // Ensure there's a block at this location that accepts an attachment
        if( me.botsko.elixr.BlockUtils.materialMeansBlockDetachment( locAcceptor.getBlock().getType() ) ) { return new ChangeResult(
                ChangeResultType.SKIPPED, null ); }

        if ((getType().getName().equals("hangingitem-break") && parameters.getProcessType().equals(PrismProcessType.ROLLBACK)) ||
            (getType().getName().equals("hangingitem-place") && parameters.getProcessType().equals(PrismProcessType.RESTORE))) {

            // We should place the ItemFrame or Painting
            try {
                if( getHangingType().equals( "item_frame" ) ) {
                    final Hanging hangingItem = getWorld().spawn( locAcceptor, ItemFrame.class );
                    hangingItem.teleport(loc);
                    hangingItem.setFacingDirection( facingDirection, true );
                    return new ChangeResult( ChangeResultType.APPLIED, null );
                } else if( getHangingType().equals( "painting" ) ) {
                    final Hanging hangingItem = getWorld().spawn( locAcceptor, Painting.class );
                    ((Painting)hangingItem).setArt(getArt(), true);
                    hangingItem.teleport(loc);
                    hangingItem.setFacingDirection( facingDirection, true );
                    return new ChangeResult( ChangeResultType.APPLIED, null );
                }
            } catch ( final IllegalArgumentException e ) {
                // Something interfered with being able to place the painting
            }

        } else if ((getType().getName().equals("hangingitem-place") && parameters.getProcessType().equals(PrismProcessType.ROLLBACK)) ||
                   (getType().getName().equals("hangingitem-break") && parameters.getProcessType().equals(PrismProcessType.RESTORE))) {

            // We should remove the ItemFrame or Painting
            Entity[] foundEntities = loc.getChunk().getEntities();
            if (foundEntities.length > 0) {
                for (Entity e : foundEntities) {
                    if (!e.getType().name().toLowerCase().equals(getHangingType()) || !loc.getWorld().equals(e.getWorld())) {
                    }
                    if (e.getType().equals(EntityType.ITEM_FRAME) && loc.equals(e.getLocation().getBlock().getLocation())) {
                        final ItemFrame frame = (ItemFrame) e;
                        if (frame.getFacing().equals(facingDirection) && frame.getItem().getType().equals(Material.AIR)) {
                            frame.remove();
                            return new ChangeResult( ChangeResultType.APPLIED, null );
                        }
                    } else if (e.getType().equals(EntityType.PAINTING)) {
                        final Painting painting = (Painting) e;

                        // Sometimes coordinates of the painting are not equal placement coordinates.
                        final double offsetY = -0.5;
                        double offsetX = 0;
                        double offsetZ = 0;
                        if (painting.getFacing().equals(BlockFace.SOUTH)) {
                            offsetX = -0.5;
                        } else if (painting.getFacing().equals(BlockFace.WEST)) {
                            offsetZ = -0.5;
                        }
                        final Location addLoc = painting.getLocation().add(offsetX, offsetY, offsetZ).getBlock().getLocation();

                        if (addLoc.equals(painting.getLocation().getBlock().getLocation()) && painting.getFacing().equals(facingDirection) &&
                                painting.getArt().equals(getArt())) {
                            painting.remove();
                            return new ChangeResult( ChangeResultType.APPLIED, null );
                        }
                    }
                }
            }

        }
        return new ChangeResult( ChangeResultType.SKIPPED, null );
    }
}