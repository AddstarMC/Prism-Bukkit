package me.botsko.prism.actions;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.Player;

public class SignAction extends GenericAction {

    public class SignChangeActionData {
        public String[] lines;
        public String sign_type;
        public BlockFace facing;
    }

    /**
	 * 
	 */
    protected Block block;

    /**
	 * 
	 */
    protected SignChangeActionData actionData;

    /**
     * 
     * @param block
     * @param lines
     */
    public void setBlock(Block block, String[] lines) {

        // Build an object for the specific details of this action
        actionData = new SignChangeActionData();

        if( block != null ) {
            actionData.sign_type = block.getType().name();
            final org.bukkit.material.Sign sign = (org.bukkit.material.Sign) block.getState().getData();
            actionData.facing = sign.getFacing();
            this.block = block;
            this.world_name = block.getWorld().getName();
            this.x = block.getX();
            this.y = block.getY();
            this.z = block.getZ();
        }
        if( lines != null ) {
            actionData.lines = lines;
        }
    }

    /**
	 * 
	 */
    @Override
    public void setData(String data) {
        this.data = data;
        if( data != null && !this.data.isEmpty() ) {
            actionData = gson.fromJson( data, SignChangeActionData.class );
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
    public String[] getLines() {
        return actionData.lines;
    }

    /**
     * 
     * @return
     */
    public Material getSignType() {
        if( actionData.sign_type != null ) {
            final Material m = Material.valueOf( actionData.sign_type );
            if( m != null ) { return m; }
        }
        return Material.SIGN;
    }

    /**
     * 
     * @return
     */
    public BlockFace getFacing() {
        return actionData.facing;
    }

    /**
     * 
     * @return
     */
    @Override
    public String getNiceName() {
        String name = "sign (";
        if( actionData.lines != null && actionData.lines.length > 0 ) {
            name += TypeUtils.join( actionData.lines, ", " );
        } else {
            name += "no text";
        }
        name += ")";
        return name;
    }

    /**
	 * 
	 */
    @Override
    public ChangeResult applyRestore(Player player, QueryParameters parameters, boolean is_preview) {

        final Block block = getWorld().getBlockAt( getLoc() );

        // Ensure a sign exists there (and no other block)
        if( block.getType().equals( Material.AIR ) || block.getType().equals( Material.SIGN_POST )
                || block.getType().equals( Material.SIGN ) || block.getType().equals( Material.WALL_SIGN ) ) {

            if( block.getType().equals( Material.AIR ) ) {
                block.setType( getSignType() );
            }

            // Set the facing direction
            if( block.getState().getData() instanceof org.bukkit.material.Sign ) {
                final org.bukkit.material.Sign s = (org.bukkit.material.Sign) block.getState().getData();
                s.setFacingDirection( getFacing() );
            }
            // Set the content
            if( block.getState() instanceof org.bukkit.block.Sign ) {

                // Set sign data
                final String[] lines = getLines();
                final org.bukkit.block.Sign sign = (org.bukkit.block.Sign) block.getState();
                int i = 0;
                if( lines != null && lines.length > 0 ) {
                    for ( final String line : lines ) {
                        sign.setLine( i, line );
                        i++;
                    }
                }
                sign.update();
                return new ChangeResult( ChangeResultType.APPLIED, null );
            }
        }
        return new ChangeResult( ChangeResultType.SKIPPED, null );
    }
}