package me.botsko.prism.actions;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.Material;
import org.bukkit.SkullType;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.block.CommandBlock;
import org.bukkit.block.CreatureSpawner;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
// MCPC+ start
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import net.minecraft.server.v1_5_R3.MinecraftServer;
import net.minecraft.server.v1_5_R3.NBTCompressedStreamTools;
import net.minecraft.server.v1_5_R3.NBTTagCompound;
import net.minecraft.server.v1_5_R3.Packet53BlockChange;
import net.minecraft.server.v1_5_R3.TileEntity;
import org.apache.commons.lang.StringEscapeUtils;
import org.bukkit.craftbukkit.v1_5_R3.CraftWorld;
// MCPC+ end

public class BlockAction extends GenericAction {
	
	/**
	 * 
	 */
	protected BlockActionData actionData;

	
	/**
	 * 
	 * @param block
	 */
	public void setBlock(Block block){
		if(block != null){
			
			block_id = BlockUtils.blockIdMustRecordAs( block.getTypeId() );
			block_subid = block.getData();
			
			// Build an object for the specific details of this action
			// @todo clean this up
			if( block_id == 144 || block_id == 397 || block_id == 52 || block_id == 63 || block_id == 68 ){
				actionData = new BlockActionData();
			}
		
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
			
			// command block
			else if( block != null && (block.getTypeId() == 137) ){
				CommandBlock cmdblock = (CommandBlock) block.getState();
				data = cmdblock.getCommand();
			}
			
			this.world_name = block.getWorld().getName();
			this.x = block.getLocation().getBlockX();
			this.y = block.getLocation().getBlockY();
			this.z = block.getLocation().getBlockZ();
		}
	}
	
	
	/**
	 * 
	 */
	@Override
	public void setData( String data ){
		this.data = data;
		if(data != null){
			if( block_id == 144 || block_id == 397 ){
				actionData = gson.fromJson(data, SkullActionData.class);
			}
			else if( block_id == 52 ){
				actionData = gson.fromJson(data, SpawnerActionData.class);
			}
			else if( block_id == 63 || block_id == 68 ){
				actionData = gson.fromJson(data, SignActionData.class);
			}
			else if( block_id == 137 ){
				actionData = new BlockActionData();
			} else {
				// No longer used except for pre-1.5 data formats
				actionData = gson.fromJson(data, BlockActionData.class);
				if( actionData.block_id > 0 ){
					this.block_id = actionData.block_id;
					this.block_subid = actionData.block_subid;
				}
			}
		}
	}

	// MCPC+ start - add method to set TileEntity data
	@Override
	public void setTileEntityData(String data) {
		this.te_data = data;
	}
	// MCPC+ end

	/**
	 * 
	 */
	@Override
	public void save(){
		// Only for the blocks we store meta data for
		if( actionData != null ){
			data = gson.toJson(actionData);
		}
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
		name += materialAliases.getAlias(this.block_id, this.block_subid);
		if(actionData instanceof SignActionData){
			SignActionData ad = (SignActionData) getActionData();
			if(ad.lines != null && ad.lines.length > 0){
				name += " (" + TypeUtils.join(ad.lines, ", ") + ")";
			}
		}
		else if( block_id == 137 ){
			name += " (" + data + ")";
		}
		if( type.getName().equals("crop-trample") && block_id == 0 ){
			return "empty soil";
		}
		return name;
	}
	
	
	/**
	 * 
	 * @author botskonet
	 *
	 */
	public class BlockActionData {
		public int block_id;
		public byte block_subid;
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
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback( Player player, QueryParameters parameters, boolean is_preview ){
		Block block = getWorld().getBlockAt( getLoc() );
		if(getType().doesCreateBlock()){
			return removeBlock(player, parameters, is_preview, block );
		} else {
			return placeBlock( player, parameters, is_preview, block, false );
		}
	}
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyRestore( Player player, QueryParameters parameters, boolean is_preview ){
		Block block = getWorld().getBlockAt( getLoc() );
		if(getType().doesCreateBlock()){
			return placeBlock( player, parameters, is_preview, block, false );
		} else {
			return removeBlock( player, parameters, is_preview, block );
		}
	}
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyUndo( Player player, QueryParameters parameters, boolean is_preview ){
		
		Block block = getWorld().getBlockAt( getLoc() );
		
		// Undo a drain/ext event (which always remove blocks)
		// @todo if we ever track rollback/restore for undo, we'll
		// need logic to do the opposite
		return placeBlock( player, parameters, is_preview, block, false );
		
	}
	

	/**
	 * 
	 */
	@Override
	public ChangeResult applyDeferred( Player player, QueryParameters parameters, boolean is_preview ){
		Block block = getWorld().getBlockAt( getLoc() );
		return placeBlock( player, parameters, is_preview, block, true );
	}
	
	
	/**
	 * Place a block unless something other than air occupies the spot, or if we detect 
	 * a falling block now sits there. This resolves
	 * the issue of falling blocks taking up the space, preventing this rollback.
	 * However, it also means that a rollback *could* interfere with a player-placed
	 * block.
	 */
	protected ChangeResult placeBlock( Player player, QueryParameters parameters, boolean is_preview, Block block, boolean is_deferred ){

		Material m = Material.getMaterial(getBlockId());
		BlockStateChange stateChange = null;

		// Ensure block action is allowed to place a block here.
		// (essentially liquid/air).
		if( !getType().requiresHandler("BlockChangeAction") && !getType().requiresHandler("PrismRollbackAction") ){
			// MCPC+ - ensure microblocks force overwrite for proper restoration
			if( !BlockUtils.isAcceptableForBlockPlace(block.getType()) && !parameters.hasFlag(Flag.OVERWRITE) && plugin.getConfig().getInt("prism.appliers.forgemultipart.block-id") != block.getTypeId() && plugin.getConfig().getInt("prism.appliers.immibismicro.block-id") != block.getTypeId()){
				return new ChangeResult( ChangeResultType.SKIPPED, null );
			}
		}
		
		// On the blacklist (except an undo)
		if( Prism.getIllegalBlocks().contains( getBlockId() ) && !parameters.getProcessType().equals(PrismProcessType.UNDO) ){
//			System.out.print("Block skipped because it's not allowed to be placed.");
			return new ChangeResult( ChangeResultType.SKIPPED, null );
		}

		// If we're not in a preview, actually apply this block
		if(!is_preview){
			
			// Capture the block before we change it
			BlockState originalBlock = block.getState();

			// If lilypad, check that block below is water. Be sure
			// it's set to stationary water so the lilypad will sit
			if(getBlockId() == 111 ){
				
				Block below = block.getRelative(BlockFace.DOWN);
				if( below.getType().equals(Material.WATER) || below.getType().equals(Material.AIR) || below.getType().equals(Material.STATIONARY_WATER) ){
					below.setType(Material.STATIONARY_WATER);
				} else {
//					Prism.debug("Lilypad skipped because no water exists below.");
					return new ChangeResult( ChangeResultType.SKIPPED, null );
				}
			}
			
			// If portal, we need to light the portal. seems to be the only way.
			if(getBlockId() == 90 ){
				Block obsidian = BlockUtils.getFirstBlockOfMaterialBelow(Material.OBSIDIAN, block.getLocation());
				if(obsidian != null){
					Block above = obsidian.getRelative(BlockFace.UP);
					if(!above.equals(Material.PORTAL)){
						above.setType(Material.FIRE);
						return new ChangeResult( ChangeResultType.APPLIED, null );
					}
				}
			}
			
			// Jukebox, never use the data val because
			// it becomes unplayable
			if( getBlockId() == 84 ){
				block_subid = 0;
			}
			
			// Set the material
			block.setTypeId(getBlockId() );
			block.setData( (byte)getBlockSubId() );
			// MCPC+ start - Get the current TileEntity NBT then compress it for storage in DB
			TileEntity tileentity = ((CraftWorld)block.getWorld()).getTileEntityAt(block.getX(), block.getY(), block.getZ());
			if (getTileEntityData() != null && !getTileEntityData().equals(""))// && !((CraftWorld)block.getWorld()).getHandle().isEmpty(block.getX(), block.getY(), block.getZ())))
			{
				NBTTagCompound c = null;
				String saved_te_data = StringEscapeUtils.unescapeJava(getTileEntityData());
				try {
					c = NBTCompressedStreamTools.a(saved_te_data.getBytes("ISO-8859-1"));
				} catch (UnsupportedEncodingException e) {
					e.printStackTrace();
				}
				if (c != null)
				{
					if (plugin.getConfig().getBoolean("prism.appliers.forgemultipart.enabled") && plugin.getConfig().getInt("prism.appliers.forgemultipart.block-id") == block.getTypeId())
					{
						try {
							Class<?> clz = Class.forName("codechicken.multipart.TileMultipart");
							Object obj = clz.newInstance();
							Method  method = clz.getDeclaredMethod ("createFromNBT", NBTTagCompound.class);
							Object returnedObj = method.invoke (obj, c);
							TileEntity te = (TileEntity)returnedObj;
							((CraftWorld)block.getWorld()).getHandle().setTileEntity(block.getX(), block.getY(), block.getZ(), te);
						} catch (ClassNotFoundException e) {
							e.printStackTrace();
						} catch (IllegalAccessException e) {
							e.printStackTrace();
						} catch (IllegalArgumentException e) {
							e.printStackTrace();
						} catch (InvocationTargetException e) {
							e.printStackTrace();
						} catch (NoSuchMethodException e) {
							e.printStackTrace();
						} catch (SecurityException e) {
							e.printStackTrace();
						} catch (InstantiationException e) {
							e.printStackTrace();
						}
					}
					else 
					{
						tileentity = TileEntity.c(c);
						((CraftWorld)block.getWorld()).getHandle().setTileEntity(block.getX(), block.getY(), block.getZ(), tileentity);
					}
				}
			}
			// MCPC+ end
			/**
			 * Skulls
			 */
			if( (getBlockId() == 144 || getBlockId() == 397) && getActionData() instanceof SkullActionData ){
				
				SkullActionData s = (SkullActionData)getActionData();
	
				// Set skull data
				Skull skull = (Skull) block.getState();
				skull.setRotation( s.getRotation() );
				skull.setSkullType( s.getSkullType() );
				if(!s.owner.isEmpty()){
					skull.setOwner( s.owner );
				}
				skull.update();
				
			}
			
			
			/**
			 * Spawner
			 */
			if(getBlockId() == 52 ){
				
				SpawnerActionData s = (SpawnerActionData)getActionData();
				
				// Set spawner data
				CreatureSpawner spawner = (CreatureSpawner) block.getState();
				spawner.setDelay(s.getDelay());
				spawner.setSpawnedType(s.getEntityType());
				spawner.update();
				
			}
			
			
			/**
			 * Restoring command block
			 */
			if(getBlockId() == 137 ){
				CommandBlock cmdblock = (CommandBlock) block.getState();
				cmdblock.setCommand( data );
				cmdblock.update();
			}
			
			
			/**
			 * Signs
			 */
			if( parameters.getProcessType().equals(PrismProcessType.ROLLBACK) && ( getBlockId() == 63 || getBlockId() == 68 ) && getActionData() instanceof SignActionData ){

				SignActionData s = (SignActionData)getActionData();
				
				// Verify block is sign. Rarely, if the block somehow pops off or fails
				// to set it causes ClassCastException: org.bukkit.craftbukkit.v1_4_R1.block.CraftBlockState 
				// cannot be cast to org.bukkit.block.Sign
				// https://snowy-evening.com/botsko/prism/455/
				if( block.getState() instanceof Sign ){
				
					// Set sign data
					Sign sign = (Sign) block.getState();
					int i = 0;
					if(s.lines != null && s.lines.length > 0){
						for(String line : s.lines){
							sign.setLine(i, line);
							i++;
						}
					}
					sign.update();
				}
			}
			
			// If the material is a crop that needs soil, we must restore the soil
			// This may need to go before setting the block, but I prefer the BlockUtil
			// logic to use materials.
			if( BlockUtils.materialRequiresSoil(block.getType()) ){
				Block below = block.getRelative(BlockFace.DOWN);
				if( below.getType().equals(Material.DIRT) || below.getType().equals(Material.AIR) || below.getType().equals(Material.GRASS) ){
					below.setType(Material.SOIL);
				} else {
//					System.out.print("Block skipped because there's no soil below.");
					return new ChangeResult( ChangeResultType.SKIPPED, null );
				}
			}
			
			// Capture the new state
			BlockState newBlock = block.getState();
			
			// Store the state change
			stateChange = new BlockStateChange(originalBlock,newBlock);
			
			// If we're rolling back a door, we need to set it properly
			if( m.equals(Material.WOODEN_DOOR) || m.equals(Material.IRON_DOOR_BLOCK) ){
				BlockUtils.properlySetDoor( block,getBlockId(),(byte)getBlockSubId());
			}
			// Or a bed
			else if( m.equals(Material.BED_BLOCK) ){
				BlockUtils.properlySetBed( block,getBlockId(),(byte)getBlockSubId());
			}
		} else {
			
			// Otherwise, save the state so we can cancel if needed
			BlockState originalBlock = block.getState();
			// Note: we save the original state as both old/new so we can re-use blockStateChanges
			stateChange = new BlockStateChange(originalBlock,originalBlock);
			
			// Preview it
			player.sendBlockChange(block.getLocation(),getBlockId(),(byte)getBlockSubId());

			// Send preview to shared players
			for(CommandSender sharedPlayer : parameters.getSharedPlayers()){
				if( sharedPlayer instanceof Player ){
					((Player)sharedPlayer).sendBlockChange(block.getLocation(),getBlockId(),(byte)getBlockSubId());
				}
			}
		}
		
		return new ChangeResult( ChangeResultType.APPLIED, stateChange );

	}
	
	
	/**
	 * 
	 */
	protected ChangeResult removeBlock( Player player, QueryParameters parameters, boolean is_preview, Block block ){
		
		BlockStateChange stateChange = null;
		
		if(!block.getType().equals(Material.AIR)){
			
			// Ensure it's acceptable to remove the current block
			if( !BlockUtils.isAcceptableForBlockPlace(block.getType()) && !BlockUtils.areBlockIdsSameCoreItem(block.getTypeId(), getBlockId()) && !parameters.hasFlag(Flag.OVERWRITE) ){
				return new ChangeResult( ChangeResultType.SKIPPED, null );
			}

			if(!is_preview){
				
				// Capture the block before we change it
				BlockState originalBlock = block.getState();
				
				// Set
				block.setType(Material.AIR);
				
				// Capture the new state
				BlockState newBlock = block.getState();
				
				// Store the state change
				stateChange = new BlockStateChange(originalBlock,newBlock);
				
			} else {
				
				// Otherwise, save the state so we can cancel if needed
				BlockState originalBlock = block.getState();
				// Note: we save the original state as both old/new so we can re-use blockStateChanges
				stateChange = new BlockStateChange(originalBlock,originalBlock);
				
				// Preview it
				player.sendBlockChange(block.getLocation(), Material.AIR, (byte)0);
				
				// Send preview to shared players
				for(CommandSender sharedPlayer : parameters.getSharedPlayers()){
					if( sharedPlayer instanceof Player ){
						((Player)sharedPlayer).sendBlockChange(block.getLocation(),getBlockId(),(byte)getBlockSubId());
					}
				}
			}
			return new ChangeResult( ChangeResultType.APPLIED, stateChange );
		}
		return new ChangeResult( ChangeResultType.SKIPPED, null );
	}
}