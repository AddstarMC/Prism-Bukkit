package me.botsko.prism.actions;

import org.bukkit.DyeColor;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Ageable;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Monster;
import org.bukkit.entity.Player;
import org.bukkit.entity.Sheep;
import org.bukkit.entity.Villager;
import org.bukkit.entity.Wolf;
import org.bukkit.entity.Villager.Profession;

public class EntityAction extends GenericAction {
	
	public class EntityActionData {
		public String entity_name;
		public boolean isAdult;
		public boolean sitting;
		public String color;
		public String newColor;
		public String profession;
		public String taming_owner;
	}
	
	/**
	 * 
	 */
	protected EntityActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param entity
	 * @param player
	 */
	public EntityAction( ActionType action_type, Entity entity, String player){
		this(action_type, entity, player, null);
	}


	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public EntityAction( ActionType action_type, Entity entity, String player, String dyeUsed ){
		
		super(action_type, player);
		
		// Build an object for the specific details of this action
		actionData = new EntityActionData();
				
		if(entity != null){
			this.actionData.entity_name = entity.getType().name().toLowerCase();
			this.world_name = entity.getWorld().getName();
			this.x = entity.getLocation().getBlockX();
			this.y = entity.getLocation().getBlockY();
			this.z = entity.getLocation().getBlockZ();
			
			// Get animal age
			if(entity instanceof Ageable && !(entity instanceof Monster) ){
				Ageable a = (Ageable)entity;
				this.actionData.isAdult = a.isAdult();
			} else {
				this.actionData.isAdult = true;
			}
			
			// Get current sheep color
			if( entity.getType().equals(EntityType.SHEEP)){
				Sheep sheep = ((Sheep) entity);
				this.actionData.color = sheep.getColor().name().toLowerCase();
			}
			
			// Get color it will become
			if(dyeUsed != null){
				this.actionData.newColor = dyeUsed;
			}
			
			// Get villager type
			if( entity instanceof Villager ){
				Villager v = (Villager)entity;
				this.actionData.profession = v.getProfession().toString().toLowerCase();
			}
			
			// Wolf details
			if (entity instanceof Wolf){
	            Wolf wolf = (Wolf)entity;
	            
	            // Owner
	            if(wolf.isTamed()){
	                if(wolf.getOwner() instanceof Player){
	                	this.actionData.taming_owner = ((Player)wolf.getOwner()).getName();
	                }
	                if(wolf.getOwner() instanceof OfflinePlayer){
	                	this.actionData.taming_owner = ((OfflinePlayer)wolf.getOwner()).getName();
	                }
	            }
	            
	            // Collar color
	            this.actionData.color = wolf.getCollarColor().name().toLowerCase();
	            
	            // Sitting
	            if( wolf.isSitting() ){
	            	this.actionData.sitting = true;
	            }
	            
	    	}
		}
		
		// Save entity data from current entity
		setDataFromObject();
		setObjectFromData();
		
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setObjectFromData();
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromObject(){
		data = gson.toJson(actionData);
	}
	
	
	/**
	 * 
	 */
	protected void setObjectFromData(){
		if(data != null){
			actionData = gson.fromJson(data, EntityActionData.class);
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public EntityType getEntityType(){
		try {
			EntityType e = EntityType.valueOf(actionData.entity_name.toUpperCase());
			if(e != null){
				return e;
			}
		} catch(IllegalArgumentException e){
			// In pre-RC builds we logged the wrong name of entities, sometimes the names
			// don't match the enum. 
		}
		return null;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isAdult(){
		return this.actionData.isAdult;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isSitting(){
		return this.actionData.sitting;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public DyeColor getColor(){
		return DyeColor.valueOf(actionData.color.toUpperCase());
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Profession getProfession(){
		return Profession.valueOf(actionData.profession.toUpperCase());
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getTamingOwner(){
		return this.actionData.taming_owner;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		if(actionData.color != null && !actionData.color.isEmpty()){
			name += actionData.color + " ";
		}
		if(actionData.isAdult && !actionData.isAdult){
			name += "baby ";
		}
		if(this.actionData.profession != null){
			name += this.actionData.profession + " ";
		}
		if(actionData.taming_owner != null){
			name += actionData.taming_owner+"'s ";
		}
		name += actionData.entity_name;
		if(this.actionData.newColor != null){
			name += " " + this.actionData.newColor;
		}
		return name;
	}
}