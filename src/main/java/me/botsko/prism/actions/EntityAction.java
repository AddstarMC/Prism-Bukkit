package me.botsko.prism.actions;

import com.google.gson.JsonObject;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.serializers.SerializationHandler;
import me.botsko.prism.serializers.entity.EntitySerializer;
import me.botsko.prism.serializers.entity.EntitySerializerFactory;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.serializers.entity.VillagerSerializer;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.event.entity.CreatureSpawnEvent;
import org.jetbrains.annotations.Nullable;

public class EntityAction extends GenericAction {

    private EntitySerializer serializer;

    /**
     * Constructor.
     * @param name String
     * @return EntityType
     */
    @Nullable
    public static EntityType getEntityType(String name) {
        try {
            return EntityType.valueOf(name.toUpperCase());
        } catch (final IllegalArgumentException e) {
            // In pre-RC builds we logged the wrong name of entities, sometimes
            // the names
            // don't match the enum.
        }
        return null;
    }

    /**
     * Set the entity.
     * @param entity Entity
     * @param dyeUsed String
     */
    public void setEntity(Entity entity, String dyeUsed) {

        // Build an object for the specific details of this action
        if (entity != null && entity.getType() != null && entity.getType().name() != null) {
            setLoc(entity.getLocation());

            serializer = EntitySerializerFactory.getSerializer(entity.getType());
            serializer.serialize(entity);
            serializer.setNewColor(dyeUsed);
        }
    }

    @Override
    public String getCustomDesc() {
        if (serializer != null) {
            return serializer.customDesc();
        }

        return null;
    }

    @Override
    public boolean hasExtraData() {
        return serializer != null;
    }

    @Override
    public String serialize() {
        return SerializationHandler.gson().toJson(serializer);
    }

    @Override
    public void deserialize(String data) {
        if (data != null && data.startsWith("{")) {
            String entityName = SerializationHandler.gson().fromJson(data, JsonObject.class).get("entityName").getAsString();
            serializer = SerializationHandler.gson().fromJson(data, EntitySerializerFactory.getSerializingClass(getEntityType(entityName)));
        }
    }

    /**
     * Get nice name.
     * @return String
     */
    @Override
    public String getNiceName() {
        if (serializer != null) {
            return serializer.toString();
        }

        return "unknown";
    }

    @Override
    public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean isPreview) {
        if (serializer == null) {
            return new ChangeResult(ChangeResultType.SKIPPED, null);
        }

        EntityType entityType = getEntityType(serializer.getEntityName());
        if (entityType != null && !Prism.getIllegalEntities().contains(entityType)) {
            if (!isPreview) {
                final Location loc = getLoc().add(0.5, 0.0, 0.5);
                if (entityType.getEntityClass() != null && loc.getWorld() != null) {
                    Prism.debug("Spawning on Rollback: " + SerializationHandler.gson().toJson(serializer));
                    Entity e = loc.getWorld().spawnEntity(loc, entityType);
                    Bukkit.getScheduler().runTaskLater(Prism.getInstance(), () -> {
                        Entity ent = Bukkit.getEntity(e.getUniqueId());
                        if (ent == null) {
                            Prism.debug("Spawned on Rollback: null");
                            return;
                        }
                        serializer.deserialize(ent);
                        VillagerSerializer out = new VillagerSerializer();
                        Entity newEnt = Bukkit.getEntity(ent.getUniqueId());
                        out.serialize(newEnt);
                        Prism.debug("Spawned on Rollback: " + SerializationHandler.gson().toJson(out));
                    },1);
                } else {
                    return new ChangeResult(ChangeResultType.SKIPPED, null);
                }
                return new ChangeResult(ChangeResultType.APPLIED, null);

            } else {
                return new ChangeResult(ChangeResultType.PLANNED, null);
            }
        } else {
            return new ChangeResult(ChangeResultType.SKIPPED, null);
        }
    }
}
