package me.botsko.prism.bridge;

import com.sk89q.worldedit.*;
import com.sk89q.worldedit.extent.inventory.BlockBag;

public class PrismBlockEditSessionFactory extends EditSessionFactory {

    /**
	 * 
	 */
    @Override
    public EditSession getEditSession(LocalWorld world, int maxBlocks, LocalPlayer player) {
        return new PrismBlockEditSession( world, maxBlocks, player );
    }

    /**
	 * 
	 */
    @Override
    public EditSession getEditSession(LocalWorld world, int maxBlocks, BlockBag blockBag, LocalPlayer player) {
        return new PrismBlockEditSession( world, maxBlocks, blockBag, player );
    }

    /**
	 * 
	 */
    public static void initialize() {
        try {
            // Check to see if the world edit version is compatible
            Class.forName( "com.sk89q.worldedit.EditSessionFactory" ).getDeclaredMethod( "getEditSession",
                    LocalWorld.class, int.class, BlockBag.class, LocalPlayer.class );
            WorldEdit.getInstance().setEditSessionFactory( new PrismBlockEditSessionFactory() );
        } catch ( final Throwable ignored ) {

        }
    }
}