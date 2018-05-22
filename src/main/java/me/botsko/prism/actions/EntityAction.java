package me.botsko.prism.actions;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.entity.EntitySerializer;
import me.botsko.prism.actions.entity.EntitySerializerFactory;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;

import org.bukkit.Location;
import org.bukkit.entity.*;

import com.google.gson.JsonObject;

public class EntityAction extends GenericAction {
	/**
	 * 
	 */
	private EntitySerializer serializer;

	/**
	 * 
	 * @param entity
	 * @param dyeUsed
	 */
	public void setEntity(Entity entity, String dyeUsed) {

		// Build an object for the specific details of this action
		if (entity != null && entity.getType() != null && entity.getType().name() != null) {
			this.world_name = entity.getWorld().getName();
			this.x = entity.getLocation().getBlockX();
			this.y = entity.getLocation().getBlockY();
			this.z = entity.getLocation().getBlockZ();

			serializer = EntitySerializerFactory.getSerializer(entity.getType());
			serializer.serialize(entity);
			serializer.setNewColor(dyeUsed);
		}
	}

	/**
	 * 
	 */
	@Override
	public void save() {
		data = gson.toJson(serializer);
	}

	/**
	 * 
	 */
	@Override
	public void setData(String data) {
		if (data != null && data.startsWith("{")) {
			String entity_name = gson.fromJson(data, JsonObject.class).get("entity_name").getAsString();
			serializer = gson.fromJson(data, EntitySerializerFactory.getSerlializingClass(getEntityType(entity_name)));
		}
	}

	/**
	 * 
	 * @return
	 */
	public static EntityType getEntityType(String name) {
		try {
			final EntityType e = EntityType.valueOf(name.toUpperCase());
			if (e != null) {
				return e;
			}
		} catch (final IllegalArgumentException e) {
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
	@Override
	public String getNiceName() {
		return serializer.toString();
	}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview) {
		EntityType entityType = getEntityType(serializer.getEntityName());
		if (entityType != null && !Prism.getIllegalEntities().contains(entityType)) {
			if (!is_preview) {
				final Location loc = getLoc().add(0.5, 0.0, 0.5);
				final Entity entity = loc.getWorld().spawnEntity(loc, entityType);

				serializer.deserialize(entity);

				return new ChangeResult(ChangeResultType.APPLIED, null);

			} else
				return new ChangeResult(ChangeResultType.PLANNED, null);
		} else
			return new ChangeResult(ChangeResultType.SKIPPED, null);
	}
}
