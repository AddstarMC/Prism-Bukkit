package me.botsko.prism.actions;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.specificentity.EntityModifier;
import me.botsko.prism.actionlibs.specificentity.EntityModifierFactory;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;

import org.bukkit.Location;
import org.bukkit.entity.*;

import com.google.gson.JsonObject;

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
	//protected EntityActionData actionData;
	private EntityModifier modifier;
	
	//public EntityActionData getActionData() {
	//	return actionData;
	//}

	/**
	 * 
	 * @param entity
	 * @param dyeUsed
	 */
	public void setEntity(Entity entity, String dyeUsed) {

		// Build an object for the specific details of this action
		//actionData = new EntityActionData();

		if(entity != null && entity.getType() != null && entity.getType().name() != null) {
			//this.actionData.entity_name = entity.getType().name().toLowerCase();
			this.world_name = entity.getWorld().getName();
			this.x = entity.getLocation().getBlockX();
			this.y = entity.getLocation().getBlockY();
			this.z = entity.getLocation().getBlockZ();
			
			// Get color it will become
			//if( dyeUsed != null ) {
			//	this.actionData.newColor = dyeUsed;
			//}
			
			modifier = EntityModifierFactory.getModifier(entity.getType()).serialize(entity);
			modifier.setNewColor(dyeUsed);
		}
	}

	/**
	 * 
	 */
	@Override
	public void save() {
		data = gson.toJson(modifier);
		//data = gson.toJson(actionData);
	}

	/**
	 * 
	 */
	@Override
	public void setData(String data) {
		if( data != null && data.startsWith( "{" ) ) {
			String entity_name = gson.fromJson(data, JsonObject.class).get("entity_name").getAsString();
			modifier = gson.fromJson(data, EntityModifierFactory.getModifierClass(getEntityType(entity_name)));
			//actionData = gson.fromJson(data, EntityActionData.class);
		}
	}

	/**
	 * 
	 * @return
	 */
	public static EntityType getEntityType(String name) {
		try {
			final EntityType e = EntityType.valueOf( name.toUpperCase() );
			if(e != null) { return e; }
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
	//public boolean isAdult() {
	//	return this.actionData.isAdult;
	//}

	/**
	 * 
	 * @return
	 */
	//public boolean isSitting() {
	//	return this.actionData.sitting;
	//}
	
	/**
	 * 
	 * @return
	 */
	//public String getTamingOwner() {
	//	return this.actionData.taming_owner;
	//}

	/**
	 * 
	 * @return
	 */
	//public String getCustomName() {
	//	return this.actionData.custom_name;
	//}

	/**
	 * 
	 * @return
	 */
	@Override
	public String getNiceName() {
		return modifier.toString();
		/*StringBuilder sb = new StringBuilder();
		
		if(actionData.taming_owner != null) {
			OfflinePlayer player = EntityUtils.offlineOf(actionData.taming_owner);
			if(player != null)
				sb.append(player.getName()).append("'s ");
		}
		
		if(actionData.profession != null)
			sb.append(actionData.profession).append(' ');
		
		if(actionData.color != null && !actionData.color.isEmpty())
			sb.append(actionData.color).append(' ');
		
		if(actionData.var != null)
			sb.append(actionData.var).append(' ');
		
		if(actionData.hColor != null)
			sb.append(actionData.hColor).append(' ');
		
		if(actionData.style != null)
			sb.append(actionData.style).append(' ');

		if(!actionData.isAdult)
			sb.append("baby ");
		
		sb.append("actionData.entity_name").append(' ');

		if(actionData.newColor != null)
			sb.append(this.actionData.newColor).append(' ');
		
		if(actionData.custom_name != null)
			sb.append("named ").append(this.actionData.custom_name);
		
		return sb.toString();*/
	}

	/**
	 *
	 * @return
	 */
	//public double getMaxHealth() {
	//	return this.actionData.maxHealth;
	//}

	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview) {
		EntityType entityType = getEntityType(modifier.getEntityName());
		if(entityType != null && !Prism.getIllegalEntities().contains(entityType)) {
			if( !is_preview ) {
				final Location loc = getLoc();

				loc.setX( loc.getX() + 0.5 );
				loc.setZ( loc.getZ() + 0.5 );

				final Entity entity = loc.getWorld().spawnEntity( loc, entityType );
				
				modifier.deserialize(entity);

				return new ChangeResult( ChangeResultType.APPLIED, null );

			}
			else
				return new ChangeResult( ChangeResultType.PLANNED, null );
		}
		else
			return new ChangeResult( ChangeResultType.SKIPPED, null );
	}
}
