package me.botsko.prism.actions.entity;

import org.bukkit.entity.EntityType;

import java.util.EnumMap;

public class EntitySerializerFactory {
    private static EntitySerializerFactory factory = null;
    private final EnumMap<EntityType, Class<? extends EntitySerializer>> entitySerializers =
            new EnumMap<>(EntityType.class);

    private EntitySerializerFactory() {
        entitySerializers.put(EntityType.HORSE, AbstractHorseSerializer.class);
        entitySerializers.put(EntityType.LLAMA, AbstractHorseSerializer.class);
        entitySerializers.put(EntityType.MULE, AbstractHorseSerializer.class);
        entitySerializers.put(EntityType.DONKEY, AbstractHorseSerializer.class);
        entitySerializers.put(EntityType.ZOMBIE_HORSE, AbstractHorseSerializer.class);
        entitySerializers.put(EntityType.SKELETON_HORSE, AbstractHorseSerializer.class);

        entitySerializers.put(EntityType.CAT, CatSerializer.class);
        entitySerializers.put(EntityType.PARROT, ParrotSerializer.class);
        entitySerializers.put(EntityType.SHEEP, SheepSerializer.class);
        entitySerializers.put(EntityType.VILLAGER, VillagerSerializer.class);
        entitySerializers.put(EntityType.WOLF, WolfSerlializer.class);
        entitySerializers.put(EntityType.ZOMBIE_VILLAGER, ZombieVillagerSerlializer.class);
        entitySerializers.put(EntityType.PANDA, PandaSerializer.class);
        /*
         * TODO: Creeper charge, enderman block, magma cube size, shulker color, slime
         * size, rabbit color
         */
    }

    static EntitySerializerFactory get() {
        if (factory != null) {
            return factory;
        }

        return (factory = new EntitySerializerFactory());
    }

    public static Class<? extends EntitySerializer> getSerlializingClass(EntityType type) {
        return get().entitySerializers.getOrDefault(type, EntitySerializer.class);
    }

    /**
     * Get a serializer class for entity.
     * @param type EntityType
     * @return EntitySerializer
     */
    public static EntitySerializer getSerializer(EntityType type) {
        Class<? extends EntitySerializer> clazz = getSerlializingClass(type);
        try {
            return clazz.getConstructor().newInstance();
        } catch (Exception e) {
            e.printStackTrace();
            throw new IllegalStateException("No default constructor found for " + clazz.getName());
        }
    }
}
