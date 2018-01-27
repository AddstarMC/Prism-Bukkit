package me.botsko.prism.actions;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.utils.EntityUtils;

import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.DyeColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.attribute.Attribute;
import org.bukkit.entity.*;
import org.bukkit.entity.Villager.Profession;
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
            
            // Owner
            if( entity instanceof Tameable ){
            	final Tameable mob = (Tameable) entity;
            	if(mob.isTamed())
            		this.actionData.taming_owner = mob.getOwner().getUniqueId().toString();
            }
            
            // Sitting
            if( entity instanceof Sittable ){
            	this.actionData.sitting = ((Sittable) entity).isSitting();
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

                // Collar color
                this.actionData.color = wolf.getCollarColor().name().toLowerCase();
            }

            // Ocelot details
            if( entity instanceof Ocelot ) {
                final Ocelot ocelot = (Ocelot) entity;

                // Cat type
                this.actionData.var = ocelot.getCatType().toString().toLowerCase();
            }

            // Horse details
            if( entity instanceof AbstractHorse ) {
                final AbstractHorse h = (AbstractHorse) entity;
                
                // TODO: Cleanup
                if(entity.getType() == EntityType.HORSE) {
                	Horse horse = (Horse)h;
                	this.actionData.hColor = horse.getColor().toString();
                    this.actionData.style  = horse.getStyle().toString();
                    setSaddle(horse.getInventory().getSaddle());
                    setArmor(horse.getInventory().getArmor());
                }
                else if(entity.getType() == EntityType.LLAMA) {
                	Llama llama = (Llama)h;
                	this.actionData.hColor = llama.getColor().toString();
                	setSaddle(llama.getInventory().getDecor());
                }
                else if(entity.getType() == EntityType.MULE || entity.getType() == EntityType.DONKEY) {
                	// Actually a saddle
                	setSaddle(h.getInventory().getItem(0));
                }
                
                if(entity instanceof ChestedHorse)
                	this.actionData.chest = ((ChestedHorse)entity).isCarryingChest();
                
                this.actionData.dom = h.getDomestication();
                this.actionData.maxDom = h.getMaxDomestication();
                this.actionData.jump = h.getJumpStrength();
                this.actionData.maxHealth = h.getAttribute(Attribute.GENERIC_MAX_HEALTH).getBaseValue();
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
        if( this.actionData.color != null )
        	try{ return DyeColor.valueOf( this.actionData.color.toUpperCase() ); }
        	catch(IllegalArgumentException e){}
        // Collars are red by default, sheep are white
    	return getEntityType() == EntityType.WOLF ? DyeColor.RED : DyeColor.WHITE;
    }

    /**
     * 
     * @return
     */
    public Profession getProfession() {
    	if( this.actionData.profession != null )
        	try{ return Profession.valueOf( this.actionData.profession.toUpperCase() ); }
        	catch(IllegalArgumentException e){}
    	return Profession.FARMER;
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
    	if( this.actionData.var != null )
        	try{ return Ocelot.Type.valueOf( this.actionData.var.toUpperCase() ); }
        	catch(IllegalArgumentException e){}
    	// Tamed vs untamed ocelot
    	return actionData.taming_owner != null ? Ocelot.Type.BLACK_CAT : Ocelot.Type.WILD_OCELOT;
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
        	UUID uuid = null;
        	try{ uuid = UUID.fromString(actionData.taming_owner); } catch(Exception e){}
        	if(uuid != null)
        		name += Bukkit.getOfflinePlayer(uuid).getName() + "'s ";
        	else
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
        if( this.actionData.hColor != null )
        	try{ return Horse.Color.valueOf( this.actionData.hColor.toUpperCase() ); }
        	catch(IllegalArgumentException e){}
        return Horse.Color.WHITE;
    }
    
    public Llama.Color getLlamaColor() {
        if( this.actionData.hColor != null )
        	try{ return Llama.Color.valueOf( this.actionData.hColor.toUpperCase() ); }
    		catch(IllegalArgumentException e){}
        return Llama.Color.CREAMY;
    }

    /**
     * 
     * @return
     */
    public Horse.Style getStyle() {
        if( this.actionData.style != null )
        	try{ return Horse.Style.valueOf( this.actionData.style.toUpperCase() ); }
        	catch(IllegalArgumentException e){}
        return Horse.Style.NONE;
    }

    /**
     * 
     * @return
     */
    public ItemStack getSaddle() {
        if( this.actionData.saddle != null ) {
        	String[] parts = this.actionData.saddle.split(":");
        	Material mat = Material.matchMaterial(parts[0]);
        	
        	if(parts.length > 1)
        		return new ItemStack(mat, 1, Short.valueOf(parts[1]));
        	else
        		return new ItemStack(mat, 1);
        }
        return null;
    }
    
    public void setSaddle(ItemStack stack) {
    	if(stack != null) {
	    	this.actionData.saddle = stack.getType().name();
	
	    	@SuppressWarnings("deprecation")
			byte data = stack.getData().getData();
	    	if(data > 0)
	    		this.actionData.saddle += ":" + data;
    	}
    }

    /**
     * 
     * @return
     */
    public ItemStack getArmor() {
        if( this.actionData.armor != null ) {
        	String[] parts = this.actionData.armor.split(":");
        	Material mat = Material.matchMaterial(parts[0]);
        	
        	if(parts.length > 1)
        		return new ItemStack(mat, 1, Short.valueOf(parts[1]));
        	else
        		return new ItemStack(mat, 1);
        }
        return null;
    }
    
    public void setArmor(ItemStack stack) {
    	if(stack != null) {
	    	this.actionData.armor = stack.getType().name();
	
	    	@SuppressWarnings("deprecation")
			byte data = stack.getData().getData();
	    	if(data > 0)
	    		this.actionData.armor += ":" + data;
    	}
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
            
            // Owner
            if( entity instanceof Tameable ){
            	((Tameable)entity).setOwner( EntityUtils.offlineOf( getTamingOwner() ) );
            }
            
            // Sitting
            if( entity instanceof Sittable ){
            	((Sittable) entity).setSitting(this.actionData.sitting);
            }

            // Set sheep color
            if( entity.getType().equals( EntityType.SHEEP ) ) {
                final Sheep sheep = ( (Sheep) entity );
                sheep.setColor( getColor() );
            }

            // Set villager profession
            if( entity instanceof Villager ) {
                final Villager v = (Villager) entity;
                v.setProfession( getProfession() );
            }

            // Set wolf details
            if( entity instanceof Wolf ) {
                final Wolf wolf = (Wolf) entity;

                // Collar color
                wolf.setCollarColor( getColor() );
            }

            // Set ocelot details
            if( entity instanceof Ocelot ) {
                final Ocelot ocelot = (Ocelot) entity;
                
                // Cat type
                ocelot.setCatType( getCatType() );

                // Sitting
                ocelot.setSitting( isSitting() );
            }

            // Set horse details
            if( entity instanceof AbstractHorse ) {
                final AbstractHorse h = (AbstractHorse) entity;

                // TODO: Check - This shouldn't be needed
                /*if( getVariant() != null ) {
                    h.setVariant( getVariant() );
                }*/
                
                if(getEntityType() == EntityType.HORSE) {
                	Horse horse = (Horse)h;
                    horse.setColor( getHorseColor() );
                    horse.setStyle( getStyle() );
                    horse.getInventory().setSaddle( getSaddle() );
                    horse.getInventory().setArmor( getArmor() );
                }
                else if(getEntityType() == EntityType.LLAMA) {
                	Llama llama = (Llama)h;
                	llama.setColor( getLlamaColor() );
                	llama.getInventory().setDecor( getSaddle() );
                }
                else if(getEntityType() == EntityType.DONKEY || getEntityType() == EntityType.MULE) {
                	h.getInventory().setItem(0, getSaddle());
                }
                
                if(entity instanceof ChestedHorse) {
                	((ChestedHorse)h).setCarryingChest( this.actionData.chest );
                }
                
                this.actionData.maxDom = Math.max(1, this.actionData.maxDom);
                this.actionData.dom = Math.min(Math.max(0, this.actionData.dom), this.actionData.maxDom);
                this.actionData.jump = Math.max(0.0, this.actionData.jump);

                h.setDomestication( this.actionData.dom );
                h.setMaxDomestication( this.actionData.maxDom );
                h.setJumpStrength( this.actionData.jump );
                h.getAttribute(Attribute.GENERIC_MAX_HEALTH).setBaseValue( this.actionData.maxHealth );
            }

            return new ChangeResult( ChangeResultType.APPLIED, null );

        }
        return new ChangeResult( ChangeResultType.PLANNED, null );
    }
}