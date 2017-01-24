package me.botsko.prism.actions;

import java.util.UUID;

import org.bukkit.DyeColor;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.attribute.Attribute;
import org.bukkit.entity.*;
import org.bukkit.entity.Horse.Variant;
import org.bukkit.entity.Villager.Profession;
import org.bukkit.inventory.HorseInventory;
import org.bukkit.inventory.ItemStack;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import org.bukkit.inventory.LlamaInventory;

public class EntityAction extends GenericAction {

    // TODO: Optimize data storage on these by making value types nullable ones
    public class EntityActionData {
        public String entity_name;
        public String custom_name;
        public boolean isAdult;
        public boolean sitting;
        public String color;
        public String newColor;
        public String profession;
        public String taming_owner;
        public UUID taming_owner_UUID;
        public String var;
        public String hColor;
        public String style;
        public boolean chest;
        public int dom;
        public int maxDom;
        public double jump;
        public String saddle;
        public String saddleData;
        public String armor;
        public double maxHealth;
        public double speed;
        public int strength;
    }

    /**
	 * 
	 */
    protected EntityActionData actionData;

    /**
     * 
     * @param entity
     * @param dyeUsed
     */
    public void setEntity(Entity entity, String dyeUsed) {

        // Build an object for the specific details of this action
        actionData = new EntityActionData();

        if( entity != null && entity.getType() != null && entity.getType().name() != null ) {
            this.actionData.entity_name = entity.getType().name().toLowerCase();
            this.world_name = entity.getWorld().getName();
            this.x = entity.getLocation().getBlockX();
            this.y = entity.getLocation().getBlockY();
            this.z = entity.getLocation().getBlockZ();

            // Get custom name
            if( entity instanceof LivingEntity ) {
                this.actionData.custom_name = ( (LivingEntity) entity ).getCustomName();
            }

            // Get animal age
            if( entity instanceof Ageable && !( entity instanceof Monster ) ) {
                final Ageable a = (Ageable) entity;
                this.actionData.isAdult = a.isAdult();
            } else {
                this.actionData.isAdult = true;
            }

            // Get current sheep color
            if( entity instanceof Sheep ) {
                final Sheep sheep = ( (Sheep) entity );
                this.actionData.color = sheep.getColor().name().toLowerCase();
            }

            // Get color it will become
            if( dyeUsed != null ) {
                this.actionData.newColor = dyeUsed;
            }

            // Get villager type
            if( entity instanceof Villager ) {
                final Villager v = (Villager) entity;
                if( v.getProfession() != null ){
                    this.actionData.profession = v.getProfession().toString().toLowerCase();
                }
            }

            // Wolf details
            if( entity instanceof Wolf ) {
                final Wolf wolf = (Wolf) entity;

                // Owner
                if( wolf.isTamed() ) {
                    if( wolf.getOwner() instanceof OfflinePlayer ) {
                        this.actionData.taming_owner = wolf.getOwner().getName();
                        this.actionData.taming_owner_UUID = wolf.getOwner().getUniqueId();
                    }
                }

                // Collar color
                this.actionData.color = wolf.getCollarColor().name().toLowerCase();

                // Sitting
                if( wolf.isSitting() ) {
                    this.actionData.sitting = true;
                }

            }

            // Ocelot details
            if( entity instanceof Ocelot ) {
                final Ocelot ocelot = (Ocelot) entity;

                // Owner
                if( ocelot.isTamed() ) {
                    if( ocelot.getOwner() instanceof OfflinePlayer ) {
                        this.actionData.taming_owner = ocelot.getOwner().getName();
                        this.actionData.taming_owner_UUID = ocelot.getOwner().getUniqueId();
                    }
                }

                // Cat type
                this.actionData.var = ocelot.getCatType().toString().toLowerCase();

                // Sitting
                if ( ocelot.isSitting() ) {
                    this.actionData.sitting = true;
                }
            }

            // Horse details
            if ( entity instanceof AbstractHorse ) {
                final AbstractHorse absHorse = (AbstractHorse) entity;
                // TODO: When rolling back, handle old and new variants
                this.actionData.dom = absHorse.getDomestication();
                this.actionData.maxDom = absHorse.getMaxDomestication();
                this.actionData.maxHealth = absHorse.getAttribute(Attribute.GENERIC_MAX_HEALTH).getBaseValue();
                this.actionData.speed = absHorse.getAttribute(Attribute.GENERIC_MOVEMENT_SPEED).getBaseValue();

                // Due to a API regression in 1.11, it's not yet possible to set saddles for
                // mules, donkeys, zombie and skeleton horses

                if ( absHorse instanceof Horse ) {
                    final Horse horse = (Horse) absHorse;
                    final HorseInventory hi = horse.getInventory();

                    this.actionData.hColor = horse.getColor().toString();
                    this.actionData.style = horse.getStyle().toString();

                    if( hi.getSaddle() != null ) {
                        this.actionData.saddle = "" + hi.getSaddle().getTypeId();
                        this.actionData.saddleData = "" + hi.getSaddle().getDurability();
                    }

                    if( hi.getArmor() != null ) {
                        this.actionData.armor = "" + hi.getArmor().getTypeId();
                    }
                }

                if ( absHorse instanceof ChestedHorse ) {
                    final ChestedHorse chestHorse = (ChestedHorse) absHorse;
                    this.actionData.chest = chestHorse.isCarryingChest();
                }

                if ( absHorse instanceof Llama ) {
                    final Llama llama = (Llama) absHorse;
                    final LlamaInventory li = llama.getInventory();

                    this.actionData.hColor = llama.getColor().toString();
                    this.actionData.strength = llama.getStrength();

                    if (li.getDecor() != null) {
                        this.actionData.saddle = "" + li.getDecor().getTypeId();
                        this.actionData.saddleData = "" + li.getDecor().getDurability();
                    }
                } else {
                    // Llama is only horse subtype without jump
                    this.actionData.jump = absHorse.getJumpStrength();
                }

                // Owner
                if( absHorse.isTamed() ) {
                    if( absHorse.getOwner() instanceof OfflinePlayer ) {
                        this.actionData.taming_owner = absHorse.getOwner().getName();
                        this.actionData.taming_owner_UUID = absHorse.getOwner().getUniqueId();
                    }
                }
            }
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
        if( data != null && data.startsWith( "{" ) ) {
            actionData = gson.fromJson( data, EntityActionData.class );
        }
    }

    /**
     * 
     * @return
     */
    public EntityType getEntityType() {
        try {
            final EntityType e = EntityType.valueOf( actionData.entity_name.toUpperCase() );

            // Transform pre-1.11 horse variants to horse entities
            if (e.equals(EntityType.HORSE) && actionData.var != null) {
                switch ( actionData.var.toUpperCase() ) {
                    case "DONKEY": return EntityType.DONKEY;
                    case "MULE": return EntityType.MULE;
                    case "UNDEAD_HORSE": return EntityType.ZOMBIE_HORSE;
                    case "SKELETON_HORSE": return EntityType.SKELETON_HORSE;
                    case "LLAMA": return EntityType.LLAMA;
                    case "HORSE":
                    default: return EntityType.HORSE;
                }
            }

            return e;
        } catch ( final IllegalArgumentException e ) {
            // In pre-RC builds we logged the wrong name of entities, sometimes
            // the names
            // don't match the enum.
        }
        return null;
    }

    /**
     * 
     * @return
     */
    public boolean isAdult() {
        return this.actionData.isAdult;
    }

    /**
     * 
     * @return
     */
    public boolean isSitting() {
        return this.actionData.sitting;
    }

    /**
     * 
     * @return
     */
    public DyeColor getColor() {
        if( actionData.color != null ) { return DyeColor.valueOf( actionData.color.toUpperCase() ); }
        return null;
    }

    /**
     * 
     * @return
     */
    public Profession getProfession() {
        if( actionData.profession != null ) { return Profession.valueOf( actionData.profession.toUpperCase() ); }
        return null;
    }

    /**
     * 
     * @return
     */
    public String getTamingOwner() {
        return this.actionData.taming_owner;
    }

    /**
     * 
     * @return
     */
    public UUID getTamingOwnerUUID() {
        return this.actionData.taming_owner_UUID;
    }

    /**
     * 
     * @return
     */
    public String getCustomName() {
        return this.actionData.custom_name;
    }

    /**
     *
     * @return
     */
    public Ocelot.Type getCatType() {
        return Ocelot.Type.valueOf( actionData.var.toUpperCase() );
    }

    /**
     * 
     * @return
     */
    @Override
    public String getNiceName() {
        String name = "";
        if( actionData.color != null && !actionData.color.isEmpty() ) {
            name += actionData.color + " ";
        }
        // if(actionData.isAdult){
        // name += "baby ";
        // }
        if( this.actionData.profession != null ) {
            name += this.actionData.profession + " ";
        }
        if( actionData.taming_owner != null ) {
            name += actionData.taming_owner + "'s ";
        }
        if( (actionData.entity_name.equals("ocelot") || actionData.entity_name.equals("horse")) && actionData.var != null ) {
            name += actionData.var.toLowerCase().replace("_", " ");
        } else {
            name += actionData.entity_name;
        }
        if( this.actionData.newColor != null ) {
            name += " " + this.actionData.newColor;
        }
        if( this.actionData.custom_name != null ) {
            name += " named " + this.actionData.custom_name;
        }
        return name;
    }

    /**
     * 
     * @return
     */
    public Variant getVariant() {
        if( !this.actionData.var.isEmpty() ) { return Variant.valueOf( this.actionData.var ); }
        return null;
    }

    /**
     *
     * @return
     */
    public Horse.Color getHorseColor() {
        if( this.actionData.hColor != null && !this.actionData.hColor.isEmpty() ) { return Horse.Color
            .valueOf( this.actionData.hColor ); }
        return null;
    }

    /**
     *
     * @return
     */
    public Llama.Color getLlamaColor() {
        if( this.actionData.hColor != null && !this.actionData.hColor.isEmpty() ) { return Llama.Color
            .valueOf( this.actionData.hColor ); }
        return null;
    }

    /**
     * 
     * @return
     */
    public Horse.Style getStyle() {
        if( !this.actionData.style.isEmpty() ) { return Horse.Style.valueOf( this.actionData.style ); }
        return null;
    }

    /**
     * 
     * @return
     */
    public ItemStack getSaddle() {
        if( this.actionData.saddle != null ) {
            short data = this.actionData.saddleData != null ? Short.parseShort(this.actionData.saddleData) : 0;
            return new ItemStack( Integer.parseInt( this.actionData.saddle ), 1, data );
        }
        return null;
    }

    /**
     * 
     * @return
     */
    public ItemStack getArmor() {
        if( this.actionData.armor != null ) { return new ItemStack( Integer.parseInt( this.actionData.armor ), 1 ); }
        return null;
    }

    /**
     *
     * @return
     */
    public double getMaxHealth() {
        return this.actionData.maxHealth;
    }

    /**
	 * 
	 */
    @Override
    public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview) {

        if( getEntityType() == null ) { return new ChangeResult( ChangeResultType.SKIPPED, null ); }

        if( Prism.getIllegalEntities().contains( getEntityType().name().toLowerCase() ) ) { return new ChangeResult(
                ChangeResultType.SKIPPED, null ); }

        if( !is_preview ) {

            final Location loc = getLoc();

            loc.setX( loc.getX() + 0.5 );
            loc.setZ( loc.getZ() + 0.5 );

            final Entity entity = loc.getWorld().spawnEntity( loc, getEntityType() );

            // Get custom name
            if( entity instanceof LivingEntity && getCustomName() != null ) {
                final LivingEntity namedEntity = (LivingEntity) entity;
                namedEntity.setCustomName( getCustomName() );
            }

            // Get animal age
            if( entity instanceof Ageable ) {
                final Ageable age = (Ageable) entity;
                if( !isAdult() ) {
                    age.setBaby();
                } else {
                    age.setAdult();
                }
            }

            // Set sheep color
            if( entity.getType().equals( EntityType.SHEEP ) && getColor() != null ) {
                final Sheep sheep = ( (Sheep) entity );
                sheep.setColor( getColor() );
            }

            // Set villager profession
            if( entity instanceof Villager && getProfession() != null ) {
                final Villager v = (Villager) entity;
                v.setProfession( getProfession() );
            }

            // Set wolf details
            if( entity instanceof Wolf ) {
            
                final Wolf wolf = (Wolf) entity;

                // Owner
            	final UUID tamingOwnerUUID = getTamingOwnerUUID();
                if (tamingOwnerUUID != null) {
                    final Player owner = plugin.getServer().getPlayer(tamingOwnerUUID);
                    if(owner == null) {
                        final OfflinePlayer offlineOwner = plugin.getServer().getOfflinePlayer(tamingOwnerUUID);
                        if (offlineOwner != null) {
                            wolf.setOwner(offlineOwner);
                        }
                    } else {
                        wolf.setOwner(owner); 
                    }
                } else {
                    final String tamingOwner = getTamingOwner();
                    if( tamingOwner != null ) {
                        Player owner = plugin.getServer().getPlayer( tamingOwner );
                        if( owner == null ) {
                            final OfflinePlayer offlinePlayer = plugin.getServer().getOfflinePlayer( tamingOwner );
                            if( offlinePlayer.hasPlayedBefore() ) {
                                owner = offlinePlayer.getPlayer();
                            }
                        }
                        if( owner != null )
                            wolf.setOwner( owner );
                    }
                }

                // Collar color
                if( getColor() != null ) {
                    wolf.setCollarColor( getColor() );
                }

                if( isSitting() ) {
                    wolf.setSitting( true );
                }
            }

            // Set ocelot details
            if( entity instanceof Ocelot ) {
            
                final Ocelot ocelot = (Ocelot) entity;

                // Owner
            	final UUID tamingOwnerUUID = getTamingOwnerUUID();
                if (tamingOwnerUUID != null) {
                    final Player owner = plugin.getServer().getPlayer(tamingOwnerUUID);
                    if(owner == null) {
                        final OfflinePlayer offlineOwner = plugin.getServer().getOfflinePlayer(tamingOwnerUUID);
                        if (offlineOwner != null) {
                        	ocelot.setOwner(offlineOwner);
                        }
                    } else {
                    	ocelot.setOwner(owner); 
                    }
                } else {
                    final String tamingOwner = getTamingOwner();
                    if( tamingOwner != null ) {
                        Player owner = plugin.getServer().getPlayer( tamingOwner );
                        if( owner == null ) {
                            final OfflinePlayer offlinePlayer = plugin.getServer().getOfflinePlayer( tamingOwner );
                            if( offlinePlayer.hasPlayedBefore() ) {
                                owner = offlinePlayer.getPlayer();
                            }
                        }
                        if( owner != null )
                        	ocelot.setOwner( owner );
                    }
                }

                // Cat type
                if( getCatType() != null ) {
                    ocelot.setCatType( getCatType() );
                }

                // Sitting
                if ( isSitting() ) {
                    ocelot.setSitting( true );
                }
            }

            // Set horse details
            if( entity instanceof AbstractHorse ) {

                final AbstractHorse absHorse = (AbstractHorse) entity;

                if ( this.actionData.dom > 0 && this.actionData.dom < this.actionData.maxDom ) {
                    absHorse.setDomestication(this.actionData.dom);
                    absHorse.setMaxDomestication( this.actionData.maxDom );
                }

                absHorse.getAttribute(Attribute.GENERIC_MAX_HEALTH).setBaseValue( this.actionData.maxHealth );
                absHorse.getAttribute(Attribute.GENERIC_MOVEMENT_SPEED).setBaseValue(this.actionData.speed);

                if ( absHorse instanceof Horse ) {
                    final Horse horse = (Horse) absHorse;
                    final HorseInventory hi = horse.getInventory();

                    if( getHorseColor() != null ) {
                        horse.setColor( getHorseColor() );
                    }

                    if( getStyle() != null ) {
                        horse.setStyle( getStyle() );
                    }

                    hi.setSaddle( getSaddle() );
                    hi.setArmor( getArmor() );
                }

                if ( absHorse instanceof ChestedHorse ) {
                    final ChestedHorse chestHorse = (ChestedHorse) absHorse;
                    chestHorse.setCarryingChest( this.actionData.chest );
                }

                if ( absHorse instanceof Llama ) {
                    final Llama llama = (Llama) absHorse;
                    final LlamaInventory li = llama.getInventory();

                    if ( getLlamaColor() != null ) {
                        llama.setColor( getLlamaColor() );
                    }

                    if ( this.actionData.strength > 0 && this.actionData.strength <= 5 ) {
                        llama.setStrength(this.actionData.strength);
                    }

                    // TODO: Report spigot bug; setDecor isn't working
                    li.setDecor( getSaddle() );
                } else {
                    // Llama is only horse subtype without jump
                    absHorse.setJumpStrength( this.actionData.jump );
                }

                // Owner
            	final UUID tamingOwnerUUID = getTamingOwnerUUID();
                if (tamingOwnerUUID != null) {
                    final Player owner = plugin.getServer().getPlayer(tamingOwnerUUID);
                    if(owner == null) {
                        final OfflinePlayer offlineOwner = plugin.getServer().getOfflinePlayer(tamingOwnerUUID);
                        if (offlineOwner != null) {
                            absHorse.setOwner(offlineOwner);
                        }
                    } else {
                        absHorse.setOwner(owner);
                    }
                } else {
                    final String tamingOwner = getTamingOwner();
                    if( tamingOwner != null ) {
                        Player owner = plugin.getServer().getPlayer( tamingOwner );
                        if( owner == null ) {
                            final OfflinePlayer offlinePlayer = plugin.getServer().getOfflinePlayer( tamingOwner );
                            if( offlinePlayer.hasPlayedBefore() ) {
                                owner = offlinePlayer.getPlayer();
                            }
                        }
                        if( owner != null )
                            absHorse.setOwner( owner );
                    }
                }
            }

            return new ChangeResult( ChangeResultType.APPLIED, null );

        }
        return new ChangeResult( ChangeResultType.PLANNED, null );
    }
}