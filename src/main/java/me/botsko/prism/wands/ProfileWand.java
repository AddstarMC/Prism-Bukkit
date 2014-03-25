package me.botsko.prism.wands;

import me.botsko.prism.Prism;

import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

public class ProfileWand extends WandBase implements Wand {

    /**
	 * 
	 */
    @Override
    public void playerLeftClick(Player player, Location loc) {
        if( loc != null ) {
            showLocationProfile( player, loc );
        }
    }

    /**
	 * 
	 */
    @Override
    public void playerRightClick(Player player, Location loc) {
        if( loc != null ) {
            showLocationProfile( player, loc );
        }
    }

    /**
     * 
     * @param player
     * @param block
     * @param loc
     */
    protected void showLocationProfile(Player player, Location loc) {

        final Block block = loc.getBlock();

        player.sendMessage( Prism.messenger.playerHeaderMsg( "Location Profile" ) );

        player.sendMessage( Prism.messenger.playerMsg( "Name: " + block.getType().toString().toLowerCase() ) );
        player.sendMessage( Prism.messenger.playerMsg( "Alias: "
                + Prism.getItems().getAlias( block.getTypeId(), block.getData() ) ) );
        player.sendMessage( Prism.messenger.playerMsg( "ID: " + block.getTypeId() + ":" + block.getData() ) );
        player.sendMessage( Prism.messenger.playerMsg( "Coords: " + block.getX() + " " + block.getY() + " "
                + block.getZ() ) );

    }

    /**
	 * 
	 */
    @Override
    public void playerRightClick(Player player, Entity entity) {
        if( entity != null ) {
            player.sendMessage( Prism.messenger.playerHeaderMsg( "Entity Profile" ) );
            player.sendMessage( Prism.messenger.playerMsg( "Name: " + entity.getType().toString().toLowerCase() ) );
            player.sendMessage( Prism.messenger.playerMsg( "ID: " + entity.getEntityId() ) );
            player.sendMessage( Prism.messenger.playerMsg( "Coords: " + entity.getLocation().getBlockX() + " "
                    + entity.getLocation().getBlockY() + " " + entity.getLocation().getBlockZ() ) );
        }
    }
}