package me.botsko.prism.actions;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.utils.EntityUtils;

import java.util.UUID;

import org.bukkit.DyeColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.attribute.Attribute;
import org.bukkit.entity.*;
import org.bukkit.entity.Villager.Profession;
import org.bukkit.inventory.HorseInventory;
import org.bukkit.inventory.ItemStack;

public class EntityAction extends GenericAction {

    public class EntityActionData {
        public String entity_name;
        public String custom_name;
        public boolean isAdult;
        public boolean sitting;
        public String color;
        public String newColor;
        public String profession;
        public String taming_owner;
        public String var;
        public String hColor;
        public String style;
        public boolean chest;
        public int dom;
        public int maxDom;
        public double jump;
        public String saddle;
        public String armor;
        public double maxHealth;
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
                    this.actionData.taming_owner = wolf.getOwner().getUniqueId().toString();
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
                    this.actionData.taming_owner = ocelot.getOwner().getUniqueId().toString();
                }

                // Cat type
                this.actionData.var = ocelot.getCatType().toString().toLowerCase();

                // Sitting
                if ( ocelot.isSitting() ) {
                    this.actionData.sitting = true;
                }
            }

            // Horse details
            if( entity instanceof Horse ) {
                final Horse h = (Horse) entity;
                
                // TODO: Cleanup
                if(entity.getType() == EntityType.HORSE)
                	this.actionData.var = "HORSE";
                else if(entity.getType() == EntityType.DONKEY)
                	this.actionData.var = "DONKEY";
                else if(entity.getType() == EntityType.MULE)
                	this.actionData.var = "MULE";
                else if(entity.getType() == EntityType.ZOMBIE_HORSE)
                	this.actionData.var = "UNDEAD_HORSE";
                else if(entity.getType() == EntityType.SKELETON_HORSE)
                	this.actionData.var = "SKELETON_HORSE";
                else if(entity.getType() == EntityType.LLAMA)
                	this.actionData.var = "LLAMA";
                
                //this.actionData.var = h.getVariant().toString();
                this.actionData.hColor = h.getColor().toString();
                this.actionData.style = h.getStyle().toString();
                this.actionData.chest = (entity instanceof ChestedHorse && ((ChestedHorse)entity).isCarryingChest());
                this.actionData.dom = h.getDomestication();
                this.actionData.maxDom = h.getMaxDomestication();
                this.actionData.jump = h.getJumpStrength();
                this.actionData.maxHealth = h.getAttribute(Attribute.GENERIC_MAX_HEALTH).getBaseValue();

                final HorseInventory hi = h.getInventory();

                if( hi.getSaddle() != null ) {
                    this.actionData.saddle = hi.getSaddle().getType().name();
                }
                if( hi.getArmor() != null ) {
                    this.actionData.armor = hi.getArmor().getType().name();
                }

                // Owner
                if( h.isTamed() ) {
                    this.actionData.taming_owner = h.getOwner().getUniqueId().toString();
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
            if( e != null ) { return e; }
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
    /*public Variant getVariant() {
        if( !this.actionData.var.isEmpty() ) { return Variant.valueOf( this.actionData.var ); }
        return null;
    }*/

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
    public Horse.Style getStyle() {
        if( !this.actionData.style.isEmpty() ) { return Horse.Style.valueOf( this.actionData.style ); }
        return null;
    }

    /**
     * 
     * @return
     */
    public ItemStack getSaddle() {
        if( this.actionData.saddle != null ) { return new ItemStack( Material.matchMaterial( this.actionData.saddle ), 1 ); }
        return null;
    }

    /**
     * 
     * @return
     */
    public ItemStack getArmor() {
        if( this.actionData.armor != null ) { return new ItemStack( Material.matchMaterial( this.actionData.armor ), 1 ); }
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

        if( Prism.getIllegalEntities().contains( getEntityType() ) ) { return new ChangeResult(
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

                // Owner
                final Wolf wolf = (Wolf) entity;
                final UUID tamingOwner = EntityUtils.uuidOf( getTamingOwner() );
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

                // Owner
                final Ocelot ocelot = (Ocelot) entity;
                final UUID tamingOwner = EntityUtils.uuidOf( getTamingOwner() );
                if( tamingOwner != null ) {
                    Player owner = plugin.getServer().getPlayer( tamingOwner );
                    if( owner == null ) {
                        final OfflinePlayer offlinePlayer = plugin.getServer().getOfflinePlayer( tamingOwner );
                        if( offlinePlayer.hasPlayedBefore() ) {
                            owner = offlinePlayer.getPlayer();
                        }
                    }
                    if( owner != null ) {
                        ocelot.setOwner(owner);
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
            if( entity instanceof Horse ) {

                final Horse h = (Horse) entity;

                // TODO: Check - This shouldn't be needed
                /*if( getVariant() != null ) {
                    h.setVariant( getVariant() );
                }*/

                if( getHorseColor() != null ) {
                    h.setColor( getHorseColor() );
                }

                if( getStyle() != null ) {
                    h.setStyle( getStyle() );
                }

                if( this.actionData.chest )
                	((ChestedHorse)h).setCarryingChest( true );
                h.setDomestication( this.actionData.dom );
                h.setMaxDomestication( this.actionData.maxDom );
                h.setJumpStrength( this.actionData.jump );
                h.getAttribute(Attribute.GENERIC_MAX_HEALTH).setBaseValue( this.actionData.maxHealth );

                // Stuff
                h.getInventory().setSaddle( getSaddle() );
                h.getInventory().setArmor( getArmor() );

                // Owner
                final UUID tamingOwner = EntityUtils.uuidOf( getTamingOwner() );
                if( tamingOwner != null ) {
                    Player owner = plugin.getServer().getPlayer( tamingOwner );
                    if( owner == null ) {
                        final OfflinePlayer offlinePlayer = plugin.getServer().getOfflinePlayer( tamingOwner );
                        if( offlinePlayer.hasPlayedBefore() ) {
                            owner = offlinePlayer.getPlayer();
                        }
                    }
                    if( owner != null )
                        h.setOwner( owner );
                }
            }

            return new ChangeResult( ChangeResultType.APPLIED, null );

        }
        return new ChangeResult( ChangeResultType.PLANNED, null );
    }
}