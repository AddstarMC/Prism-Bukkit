package me.botsko.prism.actions;

import com.google.common.base.Strings;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import org.bukkit.DyeColor;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.attribute.Attribute;
import org.bukkit.entity.*;
import org.bukkit.entity.Horse.Variant;
import org.bukkit.entity.Villager.Profession;
import org.bukkit.inventory.HorseInventory;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.LlamaInventory;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.UUID;

/** Represents an entity-related action (e.g. death, dye change) */
public class EntityAction extends GenericAction
{
    // TODO: Optimize data storage on these by making value types nullable ones
    public class EntityActionData
    {
        // All entities
        public String  entity_name;
        public String  custom_name;
        public boolean isAdult;

        // Pets
        public boolean sitting;

        // Sheep and wolf dying
        public String color;
        public String newColor;

        // Villagers
        public String profession;
        // TODO: save villager trades

        // Tameables
        public String taming_owner;
        public UUID   taming_owner_UUID;

        // Horses & llamas
        public String  var;
        public String  hColor;
        public String  style;
        public String  saddle;
        public String  saddleData;
        public String  armor;
        public boolean chest;
        public int     dom;
        public int     maxDom;
        public int     strength;
        public double  jump;
        public double  maxHealth;
        public double  speed;
    }

    /**
     * Holds entity data captured by this action
     */
    protected EntityActionData actionData;

    /**
     * Populates the given entity's data to be persisted to database by this action
     *
     * @param entity  Entity involved in this action
     * @param dyeUsed New dye applied to dyable animal, may be null
     */
    public void setEntity(Entity entity, String dyeUsed)
    {
        // Build an object for the specific details of this action
        actionData = new EntityActionData();

        if (entity == null || entity.getType() == null || entity.getType().name() == null)
            return;

        actionData.entity_name = entity.getType().name().toLowerCase();
        world_name             = entity.getWorld().getName();

        x = entity.getLocation().getBlockX();
        y = entity.getLocation().getBlockY();
        z = entity.getLocation().getBlockZ();

        // Get custom name
        if (entity instanceof LivingEntity)
            actionData.custom_name = entity.getCustomName();

        // Get animal age
        if (entity instanceof Ageable)
            actionData.isAdult = ((Ageable) entity).isAdult();
        else
            actionData.isAdult = true;

        // Get owner information
        if (entity instanceof Tameable)
        {
            final Tameable tameable = (Tameable) entity;

            if (tameable.isTamed() && tameable.getOwner() instanceof OfflinePlayer)
            {
                actionData.taming_owner      = tameable.getOwner().getName();
                actionData.taming_owner_UUID = tameable.getOwner().getUniqueId();
            }
        }

        // Get current sheep color
        if (entity instanceof Sheep)
            actionData.color = ((Sheep) entity).getColor().name().toLowerCase();

        // Get color it will become
        if (dyeUsed != null)
            actionData.newColor = dyeUsed;

        // Get villager type
        if (entity instanceof Villager)
        {
            final Villager v = (Villager) entity;

            if (v.getProfession() != null)
                actionData.profession = v.getProfession().toString().toLowerCase();
        }

        // Wolf details
        if (entity instanceof Wolf)
        {
            final Wolf wolf = (Wolf) entity;

            // Collar color
            actionData.color = wolf.getCollarColor().name().toLowerCase();

            // Sitting
            if ( wolf.isSitting() )
                actionData.sitting = true;
        }

        // Ocelot details
        if (entity instanceof Ocelot)
        {
            final Ocelot ocelot = (Ocelot) entity;

            // Cat type
            actionData.var = ocelot.getCatType().toString().toLowerCase();

            // Sitting
            if ( ocelot.isSitting() )
                actionData.sitting = true;
        }

        // Horse details
        if (entity instanceof AbstractHorse)
        {
            final AbstractHorse absHorse = (AbstractHorse) entity;

            actionData.dom    = absHorse.getDomestication();
            actionData.maxDom = absHorse.getMaxDomestication();

            actionData.maxHealth = absHorse
                .getAttribute( Attribute.GENERIC_MAX_HEALTH )
                .getBaseValue();

            actionData.speed = absHorse
                .getAttribute( Attribute.GENERIC_MOVEMENT_SPEED )
                .getBaseValue();

            // Due to a API regression in 1.11, it's not yet possible to set saddles for
            // mules, donkeys, zombie and skeleton horses

            if (absHorse instanceof Horse)
            {
                final Horse          horse = (Horse) absHorse;
                final HorseInventory hi    = horse.getInventory();

                actionData.hColor = horse.getColor().toString();
                actionData.style  = horse.getStyle().toString();

                if ( hi.getSaddle() != null )
                {
                    actionData.saddle     = "" + hi.getSaddle().getTypeId();
                    actionData.saddleData = "" + hi.getSaddle().getDurability();
                }

                if ( hi.getArmor() != null )
                    actionData.armor = "" + hi.getArmor().getTypeId();
            }

            if (absHorse instanceof ChestedHorse)
                actionData.chest = ((ChestedHorse) absHorse).isCarryingChest();

            if (absHorse instanceof Llama)
            {
                final Llama          llama = (Llama) absHorse;
                final LlamaInventory li    = llama.getInventory();

                actionData.hColor   = llama.getColor().toString();
                actionData.strength = llama.getStrength();

                if ( li.getDecor() != null )
                {
                    actionData.saddle     = "" + li.getDecor().getTypeId();
                    actionData.saddleData = "" + li.getDecor().getDurability();
                }
            }
            else
                // Llama is only horse subtype without jump
                actionData.jump = absHorse.getJumpStrength();
        }
    }

    /** Serializes data of this action, internally */
    @Override
    public void save()
    {
        data = gson.toJson( actionData );
    }

    /** Loads entity data of this action from given JSON string */
    @Override
    public void setData(String data)
    {
        if ( data != null && data.startsWith( "{" ) )
            actionData = gson.fromJson( data, EntityActionData.class );
    }

    /** @return Bukkit type of this action's entity, or null if invalid */
    @Nullable
    public EntityType getEntityType()
    {
        try
        {
            final EntityType e = EntityType.valueOf( actionData.entity_name.toUpperCase() );

            // Transform pre-1.11 horse variants to horse entities
            if (e.equals(EntityType.HORSE) && actionData.var != null)
            {
                switch ( actionData.var.toUpperCase() )
                {
                    case "DONKEY":
                        return EntityType.DONKEY;
                    case "MULE":
                        return EntityType.MULE;
                    case "UNDEAD_HORSE":
                        return EntityType.ZOMBIE_HORSE;
                    case "SKELETON_HORSE":
                        return EntityType.SKELETON_HORSE;
                    case "LLAMA":
                        return EntityType.LLAMA;
                    case "HORSE":
                    default:
                        return EntityType.HORSE;
                }
            }

            return e;
        }
        catch (final IllegalArgumentException e)
        {
            // In pre-RC builds we logged the wrong name of entities, sometimes the names don't
            // match the enum.
        }
        return null;
    }

    /** @return true if action's entity is adult, false otherwise */
    public boolean isAdult()
    {
        return actionData.isAdult;
    }

    /** @return true if action's entity is sitting, false otherwise */
    public boolean isSitting()
    {
        return actionData.sitting;
    }

    /** @return Bukkit dye color of this action's entity */
    @Nullable
    public DyeColor getColor()
    {
        return !Strings.isNullOrEmpty(actionData.color)
            ? DyeColor.valueOf( actionData.color.toUpperCase() )
            : null;
    }

    /** @return Bukkit profession of this action's villager */
    @Nullable
    public Profession getProfession()
    {
        return !Strings.isNullOrEmpty(actionData.profession)
            ? Profession.valueOf( actionData.profession.toUpperCase() )
            : null;
    }

    /** @return Owner's name of this action's entity */
    @Nullable
    public String getTamingOwner()
    {
        return actionData.taming_owner;
    }

    /** @return Owner's UUID of this action's entity */
    @Nullable
    public UUID getTamingOwnerUUID()
    {
        return actionData.taming_owner_UUID;
    }

    /** @return Custom name this action's entity */
    @Nullable
    public String getCustomName()
    {
        return actionData.custom_name;
    }

    /** @return Bukkit type of this action's cat */
    @Nullable
    public Ocelot.Type getCatType()
    {
        return actionData.var != null
            ? Ocelot.Type.valueOf( actionData.var.toUpperCase() )
            : null;
    }
    }

    /** @return Fancy name for this entity, based on its data */
    @Nonnull
    @Override
    public String getNiceName()
    {
        String name = "";

        if ( !Strings.isNullOrEmpty(actionData.color) )
            name += actionData.color + " ";

        if (!actionData.isAdult)
            name += "baby ";

        if ( !Strings.isNullOrEmpty(actionData.profession) )
            name += actionData.profession + " ";

        if ( !Strings.isNullOrEmpty(actionData.taming_owner) )
            name += actionData.taming_owner + "'s ";

        if ( (actionData.entity_name.equals("ocelot") || actionData.entity_name.equals("horse"))
            && actionData.var != null )
            name += actionData.var.toLowerCase().replace( "_", " " );
        else
            name += actionData.entity_name;

        if ( !Strings.isNullOrEmpty(actionData.newColor) )
            name += " " + actionData.newColor;

        if ( !Strings.isNullOrEmpty(actionData.custom_name) )
            name += " named " + actionData.custom_name;

        return name;
    }

    /**
     * 1.11 separates horse variants into individual types. This is only for older pre-1.11 records.
     * @return Bukkit horse variant of this action's horse
     */
    @Deprecated
    @Nullable
    public Variant getVariant()
    {
        return !Strings.isNullOrEmpty(actionData.var)
            ? Variant.valueOf(actionData.var)
            : null;
    }

    /** @return Bukkit horse color of this action's horse */
    @Nullable
    public Horse.Color getHorseColor()
    {
        return !Strings.isNullOrEmpty(actionData.hColor)
            ? Horse.Color.valueOf(actionData.hColor)
            : null;
    }

    /** @return Bukkit llama color of this action's llama */
    @Nullable
    public Llama.Color getLlamaColor()
    {
        return !Strings.isNullOrEmpty(actionData.hColor)
            ? Llama.Color.valueOf(actionData.hColor)
            : null;
    }

    /** @return Bukkit horse style of this action's horse */
    @Nullable
    public Horse.Style getStyle()
    {
        return !Strings.isNullOrEmpty(actionData.style)
            ? Horse.Style.valueOf(actionData.style)
            : null;
    }

    /** @return Bukkit item stack of this action's entity's saddle */
    @Nullable
    public ItemStack getSaddle()
    {
        if ( Strings.isNullOrEmpty(actionData.saddle) )
            return null;

        short data = actionData.saddleData != null
            ? Short.parseShort( actionData.saddleData )
            : 0;

        return new ItemStack(Integer.parseInt(actionData.saddle), 1, data);
    }

    /** @return Bukkit item stack of this action's entity's armor */
    @Nullable
    public ItemStack getArmor()
    {
        return !Strings.isNullOrEmpty(actionData.armor)
            ? new ItemStack(Integer.parseInt(actionData.armor), 1)
            : null;
    }

    /** @return Max health of this action's entity, may be 0 */
    public double getMaxHealth()
    {
        return actionData.maxHealth;
    }

    /** Handles rolling back of entity actions */
    @Override
    public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview)
    {
        // Previewing unsupported
        if (is_preview)
            return new ChangeResult(ChangeResultType.PLANNED, null);

        EntityType entityType = getEntityType();

        if (entityType == null)
            return new ChangeResult(ChangeResultType.SKIPPED, null);

        if ( Prism.getIllegalEntities().contains( entityType.name().toLowerCase() ) )
            return new ChangeResult(ChangeResultType.SKIPPED, null);

        final Location loc = getLoc();

        loc.setX( loc.getX() + 0.5 );
        loc.setZ( loc.getZ() + 0.5 );

        final Entity entity = loc.getWorld().spawnEntity( loc, getEntityType() );

        // Set custom name
        if (getCustomName() != null)
            entity.setCustomName( getCustomName() );

        // Set animal age
        if (entity instanceof Ageable)
        {
            final Ageable age = (Ageable) entity;

            if ( isAdult() ) age.setAdult();
            else             age.setBaby();
        }

        // Set owner information
        if (entity instanceof Tameable)
        {
            final Tameable tameable        = (Tameable) entity;
            final UUID     tamingOwnerUUID = getTamingOwnerUUID();

            if (tamingOwnerUUID != null)
            {
                final Player owner = plugin.getServer().getPlayer(tamingOwnerUUID);

                if (owner == null)
                {
                    final OfflinePlayer offlineOwner = plugin
                        .getServer()
                        .getOfflinePlayer(tamingOwnerUUID);

                    if (offlineOwner != null)
                        tameable.setOwner(offlineOwner);
                }
                else
                    tameable.setOwner(owner);
            }
        }

        // Set sheep color
        if (entity instanceof Sheep && getColor() != null)
            ((Sheep) entity).setColor( getColor() );

        // Set villager profession
        if (entity instanceof Villager && getProfession() != null)
            ((Villager) entity).setProfession( getProfession() );

        // Set wolf details
        if (entity instanceof Wolf)
        {
            final Wolf wolf = (Wolf) entity;

            // Collar color
            if ( getColor() != null )
                wolf.setCollarColor( getColor() );

            if ( isSitting() )
                wolf.setSitting(true);
        }

        // Set ocelot details
        if (entity instanceof Ocelot)
        {
            final Ocelot ocelot = (Ocelot) entity;

            // Cat type
            if ( getCatType() != null )
                ocelot.setCatType( getCatType() );

            // Sitting
            if ( isSitting() )
                ocelot.setSitting(true);
        }

        // Set horse details
        if (entity instanceof AbstractHorse)
        {
            final AbstractHorse absHorse = (AbstractHorse) entity;

            if (actionData.dom > 0 && actionData.dom < actionData.maxDom)
            {
                absHorse.setDomestication(actionData.dom);
                absHorse.setMaxDomestication(actionData.maxDom);
            }

            if (actionData.maxHealth > 0)
                absHorse
                    .getAttribute(Attribute.GENERIC_MAX_HEALTH)
                    .setBaseValue(actionData.maxHealth);

            if (actionData.speed > 0)
                absHorse
                    .getAttribute(Attribute.GENERIC_MOVEMENT_SPEED)
                    .setBaseValue(actionData.speed);

            if (absHorse instanceof Horse)
            {
                final Horse          horse = (Horse) absHorse;
                final HorseInventory hi    = horse.getInventory();

                if (getHorseColor() != null)
                    horse.setColor( getHorseColor() );

                if (getStyle() != null)
                    horse.setStyle( getStyle() );

                hi.setSaddle( getSaddle() );
                hi.setArmor( getArmor() );
            }

            if (absHorse instanceof ChestedHorse)
            {
                final ChestedHorse chestHorse = (ChestedHorse) absHorse;
                chestHorse.setCarryingChest(actionData.chest);
            }

            if (absHorse instanceof Llama)
            {
                final Llama llama = (Llama) absHorse;
                final LlamaInventory li = llama.getInventory();

                if (getLlamaColor() != null)
                    llama.setColor( getLlamaColor() );

                if (actionData.strength > 0 && actionData.strength <= 5)
                    llama.setStrength( actionData.strength );

                // TODO: Report spigot bug; setDecor isn't working
                li.setDecor( getSaddle() );
            }
            else
                // Llama is only horse subtype without jump
                absHorse.setJumpStrength( actionData.jump );
        }

        return new ChangeResult(ChangeResultType.APPLIED, null);
    }
}