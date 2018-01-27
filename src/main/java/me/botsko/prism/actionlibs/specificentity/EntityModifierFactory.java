package me.botsko.prism.actionlibs.specificentity;

import java.util.EnumMap;

import org.bukkit.entity.EntityType;

public class EntityModifierFactory {
	private static EntityModifierFactory factory = null;
	
	static EntityModifierFactory get() {
		if(factory != null)
			return factory;
		
		return (factory = new EntityModifierFactory());
	}

	private EnumMap<EntityType, Class<? extends EntityModifier>> entityModifiers = new EnumMap<>(EntityType.class);
	
	private EntityModifierFactory() {
		entityModifiers.put(EntityType.HORSE, AbstractHorseModifier.class);
		entityModifiers.put(EntityType.LLAMA, AbstractHorseModifier.class);
		entityModifiers.put(EntityType.MULE, AbstractHorseModifier.class);
		entityModifiers.put(EntityType.DONKEY, AbstractHorseModifier.class);
		entityModifiers.put(EntityType.ZOMBIE_HORSE, AbstractHorseModifier.class);
		entityModifiers.put(EntityType.SKELETON_HORSE, AbstractHorseModifier.class);

		entityModifiers.put(EntityType.OCELOT, OcelotModifier.class);
		entityModifiers.put(EntityType.PARROT, ParrotModifier.class);
		entityModifiers.put(EntityType.SHEEP, SheepModifier.class);
		entityModifiers.put(EntityType.VILLAGER, VillagerModifier.class);
		entityModifiers.put(EntityType.WOLF, WolfModifier.class);
		entityModifiers.put(EntityType.ZOMBIE_VILLAGER, ZombieVillagerModifier.class);
		
		/*
		 * TODO:
		 * Creeper charge, enderman block, magma cube size,
		 * shulker color, slime size, rabbit color
		 * 
		 * all entities with armor ???
		 */
	}
	
	public static Class<? extends EntityModifier> getModifierClass(EntityType type) {
		return get().entityModifiers.getOrDefault(type, EntityModifier.class);
	}
	
	public static EntityModifier getModifier(EntityType type) {
		Class<? extends EntityModifier> clazz = getModifierClass(type);
		try {
			return clazz.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			e.printStackTrace();
			throw new IllegalStateException("No default constructor for " + clazz.getName());
		}
	}
}
