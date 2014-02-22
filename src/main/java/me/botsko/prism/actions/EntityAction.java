package me.botsko.prism.actions;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;

import org.bukkit.DyeColor;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Ageable;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Horse;
import org.bukkit.entity.Horse.Variant;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Monster;
import org.bukkit.entity.Player;
import org.bukkit.entity.Sheep;
import org.bukkit.entity.Villager;
import org.bukkit.entity.Wolf;
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
	public void setEntity( Entity entity, String dyeUsed ){
		
		// Build an object for the specific details of this action
		actionData = new EntityActionData();
				
		if( entity != null && entity.getType() != null && entity.getType().name() != null ){
			this.actionData.entity_name = entity.getType().name().toLowerCase();
			this.world_name = entity.getWorld().getName();
			this.x = entity.getLocation().getBlockX();
			this.y = entity.getLocation().getBlockY();
			this.z = entity.getLocation().getBlockZ();
			
			
			// Get custom name
			if( entity instanceof LivingEntity ){
				this.actionData.custom_name = ((LivingEntity)entity).getCustomName();
			}
			
			// Get animal age
			if(entity instanceof Ageable && !(entity instanceof Monster) ){
				Ageable a = (Ageable)entity;
				this.actionData.isAdult = a.isAdult();
			} else {
				this.actionData.isAdult = true;
			}
			
			// Get current sheep color
			if( entity instanceof Sheep ){
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
			
			// Horse details
			if( entity instanceof Horse ){
				Horse h = (Horse) entity;
				this.actionData.var = h.getVariant().toString();
				this.actionData.hColor = h.getColor().toString();
				this.actionData.style = h.getStyle().toString();
				this.actionData.chest = h.isCarryingChest();
				this.actionData.dom = h.getDomestication();
				this.actionData.maxDom = h.getMaxDomestication();
				this.actionData.jump = h.getJumpStrength();
				
				HorseInventory hi = h.getInventory();
				
				if( hi.getSaddle() != null ){
					this.actionData.saddle = ""+hi.getSaddle().getTypeId();
				}
				if( hi.getArmor() != null ){
					this.actionData.armor = ""+hi.getArmor().getTypeId();
				}
				
				// Owner
	            if(h.isTamed()){
	                if(h.getOwner() instanceof Player){
	                	this.actionData.taming_owner = ((Player)h.getOwner()).getName();
	                }
	                if(h.getOwner() instanceof OfflinePlayer){
	                	this.actionData.taming_owner = ((OfflinePlayer)h.getOwner()).getName();
	                }
	            }
			}
		}
	}
	
	
	/**
	 * 
	 */
	public void save(){
		data = gson.toJson(actionData);
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		if( data != null && data.startsWith("{") ){
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
		if(actionData.color != null){
			return DyeColor.valueOf(actionData.color.toUpperCase());
		}
		return null;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Profession getProfession(){
		if(actionData.profession != null){
			return Profession.valueOf(actionData.profession.toUpperCase());
		}
		return null;
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
	public String getCustomName(){
		return this.actionData.custom_name;
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
		if(this.actionData.custom_name != null){
			name += " named " + this.actionData.custom_name;
		}
		return name;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Variant getVariant(){
		if( !this.actionData.var.isEmpty() ){
			return Variant.valueOf( this.actionData.var );
		}
		return null;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Horse.Color getHorseColor(){
		if( this.actionData.hColor != null && !this.actionData.hColor.isEmpty() ){
			return Horse.Color.valueOf( this.actionData.hColor );
		}
		return null;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Horse.Style getStyle(){
		if( !this.actionData.style.isEmpty() ){
			return Horse.Style.valueOf( this.actionData.style );
		}
		return null;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public ItemStack getSaddle(){
		if( this.actionData.saddle != null ){
			return new ItemStack( Integer.parseInt( this.actionData.saddle ), 1 );
		}
		return null;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public ItemStack getArmor(){
		if( this.actionData.armor != null ){
			return new ItemStack( Integer.parseInt( this.actionData.armor ), 1 );
		}
		return null;
	}
	
	
	/**
	 * 
	 */
	public ChangeResult applyRollback( Player player, QueryParameters parameters, boolean is_preview ){
		
		if(getEntityType() == null){
			return new ChangeResult( ChangeResultType.SKIPPED, null );
		}
		
		if( Prism.getIllegalEntities().contains( getEntityType().name().toLowerCase() ) ){
			return new ChangeResult( ChangeResultType.SKIPPED, null );
		}
		
		if( !is_preview ){
		
			Location loc = getLoc();
			
			loc.setX( loc.getX()+0.5 );
			loc.setZ( loc.getZ()+0.5 );
			
			Entity entity = loc.getWorld().spawnEntity(loc, getEntityType());
			
			// Get custom name
			if( entity instanceof LivingEntity && getCustomName() != null ){
				LivingEntity namedEntity = (LivingEntity)entity;
				namedEntity.setCustomName( getCustomName() );
			}
			
			// Get animal age
			if(entity instanceof Ageable){
				Ageable age = (Ageable)entity;
				if(!isAdult()){
					age.setBaby();
				}
			}
			
			// Set sheep color
			if( entity.getType().equals(EntityType.SHEEP) && getColor() != null ){
				Sheep sheep = ((Sheep) entity);
				sheep.setColor( getColor() );
			}
			
			// Set villager profession
			if( entity instanceof Villager && getProfession() != null ){
				Villager v = (Villager)entity;
				v.setProfession( getProfession() );
			}
			
			// Set wolf details
			if (entity instanceof Wolf){
				
				// Owner
	            Wolf wolf = (Wolf)entity;
	            String tamingOwner = getTamingOwner();
	            if(tamingOwner != null){
		            Player owner = plugin.getServer().getPlayer( tamingOwner );
		            if(owner == null){
			            OfflinePlayer offlinePlayer = plugin.getServer().getOfflinePlayer( tamingOwner );
			            if(offlinePlayer.hasPlayedBefore()){
			            	owner = offlinePlayer.getPlayer();
			            }
		            }
		            if(owner != null) wolf.setOwner(owner);
	            }
	            
	            // Collar color
	            if( getColor() != null ){
	            	wolf.setCollarColor( getColor() );
	            }
	            
	            if(isSitting()){
	            	wolf.setSitting(true);
	            }
	    	}
			
			// Set horse details
			if( entity instanceof Horse ){
				
				Horse h = (Horse) entity;
				
				if( getVariant() != null ){
					h.setVariant( getVariant() );
				}
				
				if( getHorseColor() != null ){
					h.setColor( getHorseColor() );
				}
				
				if( getStyle() != null ){
					h.setStyle( getStyle() );
				}
				
				h.setCarryingChest( this.actionData.chest );
				h.setDomestication( this.actionData.dom );
				h.setMaxDomestication( this.actionData.maxDom );
				h.setJumpStrength( this.actionData.jump );
				
				// Stuff
				h.getInventory().setSaddle( getSaddle() );
				h.getInventory().setArmor( getArmor() );
				
				// Owner
	            String tamingOwner = getTamingOwner();
	            if(tamingOwner != null){
		            Player owner = plugin.getServer().getPlayer( tamingOwner );
		            if(owner == null){
			            OfflinePlayer offlinePlayer = plugin.getServer().getOfflinePlayer( tamingOwner );
			            if(offlinePlayer.hasPlayedBefore()){
			            	owner = offlinePlayer.getPlayer();
			            }
		            }
		            if(owner != null) h.setOwner(owner);
	            }
			}

			return new ChangeResult( ChangeResultType.APPLIED, null );
			
		}
		return new ChangeResult( ChangeResultType.PLANNED, null );
	}
}