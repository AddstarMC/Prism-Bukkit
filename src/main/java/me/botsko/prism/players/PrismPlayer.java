package me.botsko.prism.players;

import java.util.UUID;

import org.bukkit.entity.Player;

public class PrismPlayer {
    
    private int playerId;
    private String player;
    private UUID playerUuid;
    

    /**
     * 
     * @param playerId
     * @param player
     */
    public PrismPlayer( int playerId, Player player ){
        this( playerId, player.getUniqueId(), player.getName() );
    }
    
    
    /**
     * 
     * @param playerId
     * @param playerUuid
     * @param player
     */
    public PrismPlayer( int playerId, UUID playerUuid, String player ){
        this.playerId = playerId;
        this.playerUuid = playerUuid;
        this.player = player;
    }
    
    
    /**
     * 
     * @return
     */
    public void setId( int newId ){
        if( playerId > 0 ) throw new IllegalArgumentException("Cannot overwrite PrismPlayer primary key.");
        playerId = newId;
    }
    
    
    /**
     * 
     * @return
     */
    public int getId(){
        return playerId;
    }
    
    
    /**
     * 
     */
    public String getName(){
        return player;
    }
    
    
    /**
     * 
     */
    public void setName( String name ){
        player = name;
    }
    
    
    /**
     * 
     */
    public UUID getUUID(){
        return playerUuid;
    }
    
    
    /**
     * 
     */
    public void setUUID( UUID uuid ){
        playerUuid = uuid;
    }
}