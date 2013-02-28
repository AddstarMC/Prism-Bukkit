package me.botsko.prism.actions;

import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.TypeUtils;

import org.bukkit.SkullType;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.CreatureSpawner;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;
import org.bukkit.entity.EntityType;

public class BlockAction extends GenericAction {
	
	/**
	 * 
	 */
	protected BlockActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public BlockAction( ActionType action_type, Block block, String player ){
		
		super(action_type, player);
		
		// Build an object for the specific details of this action
		actionData = new BlockActionData();
		
		if(block != null){
		
			// spawner
			if( block != null && block.getTypeId() == 52 ){
				SpawnerActionData spawnerActionData = new SpawnerActionData();
				CreatureSpawner s = (CreatureSpawner)block.getState();
				spawnerActionData.entity_type = s.getSpawnedType().name().toLowerCase();
				spawnerActionData.delay = s.getDelay();
				actionData = spawnerActionData;
			}
			
			// skulls
			else if( block != null && (block.getTypeId() == 144 || block.getTypeId() == 397) ){
				SkullActionData skullActionData = new SkullActionData();
				Skull s = (Skull)block.getState();
				skullActionData.rotation = s.getRotation().name().toLowerCase();
				skullActionData.owner = s.getOwner();
				skullActionData.skull_type = s.getSkullType().name().toLowerCase();
				actionData = skullActionData;
			}
			
			// signs
			else if( block != null && (block.getTypeId() == 63 || block.getTypeId() == 68) ){
				SignActionData signActionData = new SignActionData();
				Sign s = (Sign) block.getState();
				signActionData.lines = s.getLines();
				actionData = signActionData;
			}
			
			actionData.block_id = BlockUtils.blockIdMustRecordAs( block.getTypeId() );
			actionData.block_subid = block.getData();
			this.world_name = block.getWorld().getName();
			this.x = block.getLocation().getBlockX();
			this.y = block.getLocation().getBlockY();
			this.z = block.getLocation().getBlockZ();
		}
		
		// Set data from current block
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
	 * @return
	 */
	public BlockActionData getActionData(){
		return actionData;
	}
	
	
	/**
	 * 
	 */
	protected void setObjectFromData(){
		if(data != null){
			if( data.contains("block_id\":144,") || data.contains("block_id\":397,") ){
				actionData = gson.fromJson(data, SkullActionData.class);
			}
			else if( data.contains("block_id\":52,") ){
				actionData = gson.fromJson(data, SpawnerActionData.class);
			}
			else if( data.contains("block_id\":63,") || data.contains("block_id\":68,") ){
				actionData = gson.fromJson(data, SignActionData.class);
			}
			else {
				actionData = gson.fromJson(data, BlockActionData.class);
			}
		}
	}
	
	
	/**
	 * 
	 * @param id
	 */
	public void setBlockId( int id ){
		actionData.block_id = id;
		setDataFromObject();
	}
	
	
	/**
	 * 
	 * @param id
	 */
	public void setBlockSubId( byte id ){
		actionData.block_subid = id;
		setDataFromObject();
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		if(actionData instanceof SkullActionData){
			SkullActionData ad = (SkullActionData) getActionData();
			name += ad.skull_type + " ";
		}
		else if(actionData instanceof SpawnerActionData){
			SpawnerActionData ad = (SpawnerActionData) getActionData();
			name += ad.entity_type + " ";
		}
		name += materialAliases.getItemStackAliasById(actionData.block_id, actionData.block_subid);
		if(actionData instanceof SignActionData){
			SignActionData ad = (SignActionData) getActionData();
			if(ad.lines != null && ad.lines.length > 0){
				name += " (" + TypeUtils.implode(ad.lines, ", ") + ")";
			}
		}
		return name;
	}
	
	
	/**
	 * 
	 * @author botskonet
	 */
	public class BlockActionData {
		
		public int block_id;
		public byte block_subid;
		
		/**
		 * 
		 */
		public int getBlockId(){
			return block_id;
		}
		
		
		/**
		 * 
		 */
		public byte getBlockSubid(){
			return block_subid;
		}
	}
	
	
	/**
	 * 
	 * @author botskonet
	 */
	public class SpawnerActionData extends BlockActionData {
		
		public String entity_type;
		public int delay;
		
		/**
		 * 
		 * @return
		 */
		public EntityType getEntityType(){
			return EntityType.valueOf(entity_type.toUpperCase());
		}
		
		
		/**
		 * 
		 * @return
		 */
		public int getDelay(){
			return delay;
		}
	}
	
	
	/**
	 * 
	 * @author botskonet
	 */
	public class SkullActionData extends BlockActionData {
		
		public String rotation;
		public String owner;
		public String skull_type;
		
		
		/**
		 * 
		 * @return
		 */
		public SkullType getSkullType(){
			if(skull_type != null){
				return SkullType.valueOf(skull_type.toUpperCase());
			}
			return null;
		}
		
		
		/**
		 * 
		 * @return
		 */
		public BlockFace getRotation(){
			if(rotation != null){
				return BlockFace.valueOf(rotation.toUpperCase());
			}
			return null;
		}
	}
	
	
	/**
	 * Not to be confused with SignChangeActionData, which records
	 * additional data we don't need here.
	 * @author botskonet
	 *
	 */
	public class SignActionData extends BlockActionData {
		public String[] lines;
	}
}