package me.botsko.prism.actions;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import me.botsko.elixr.InventoryUtils;
import me.botsko.elixr.TypeUtils;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.utils.ItemUtils;

import org.bukkit.Color;
import org.bukkit.FireworkEffect;
import org.bukkit.FireworkEffect.Builder;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BrewingStand;
import org.bukkit.block.Chest;
import org.bukkit.block.Dispenser;
import org.bukkit.block.Dropper;
import org.bukkit.block.Furnace;
import org.bukkit.block.Hopper;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.BookMeta;
import org.bukkit.inventory.meta.EnchantmentStorageMeta;
import org.bukkit.inventory.meta.FireworkEffectMeta;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.LeatherArmorMeta;
import org.bukkit.inventory.meta.SkullMeta;

public class ItemStackAction extends GenericAction {
	
	public class ItemStackActionData {
		public int block_id; // only for pre-1.5 compat
		public int block_subid; // only for pre-1.5 compat
		public int amt;
		public String name;
		public int color;
		public String owner;
		public String[] enchs;
		public String by;
		public String title;
		public int slot = -1;
		public int[] effectColors;
		public int[] fadeColors;
		public boolean hasFlicker;
		public boolean hasTrail;
	}
	
	/**
	 * 
	 */
	protected ItemStack item;
	
	/**
	 * 
	 */
	protected ItemStackActionData actionData;
	
	/**
	 * We store the enchantments here because an event like item enchant
	 * doesn't give us the item with the enchantments already on it.
	 */
	protected Map<Enchantment,Integer> enchantments;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public void setItem( ItemStack item, int quantity, int slot, Map<Enchantment,Integer> enchantments ){
		
		actionData = new ItemStackActionData();
		
		if(enchantments != null){
			this.enchantments = enchantments;
		}
		
		if( item == null || item.getAmount() <= 0 ){
			this.setCanceled(true);
			return;
		}
			
		this.item = item;
		if(enchantments == null){
			this.enchantments = item.getEnchantments();
		}
		
		// Set basics
		this.block_id = item.getTypeId();
		this.block_subid = item.getDurability();
		actionData.amt = quantity;
		if(slot >= 0){
			actionData.slot = slot;
		}
		
		// Set additional data all items may have
		ItemMeta meta = item.getItemMeta();
		if(meta.getDisplayName() != null){
			actionData.name = meta.getDisplayName();
		}

		// Leather Coloring
		if(item.getType().name().contains("LEATHER_")){
			LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
			if(lam.getColor() != null){
				actionData.color = lam.getColor().asRGB();
			}
		}
		
		// Skull Owner
		else if(item.getType().equals(Material.SKULL_ITEM)){
			SkullMeta skull = (SkullMeta) item.getItemMeta();
			if(skull.hasOwner()){
				actionData.owner = skull.getOwner();
			}
		}
		
		// Written books
		if(item.getType().equals( Material.WRITTEN_BOOK )){
	        BookMeta bookMeta = (BookMeta) item.getItemMeta();
			if(bookMeta != null){
				actionData.by = bookMeta.getAuthor();
				actionData.title = bookMeta.getTitle();
			}
		}
		
		// Enchantments
		if(!this.enchantments.isEmpty()){
			String[] enchs = new String[this.enchantments.size()];
			int i = 0;
			for(Entry<Enchantment, Integer> ench : this.enchantments.entrySet()){
				enchs[i] = ench.getKey().getId() + ":" + ench.getValue();
				i++;
			}
			actionData.enchs = enchs;
		}

		// Book enchantments
		else if(item.getType().equals( Material.ENCHANTED_BOOK )){
			EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
			if(bookEnchantments.hasStoredEnchants()){
				if(bookEnchantments.getStoredEnchants().size() > 0){
					String[] enchs = new String[bookEnchantments.getStoredEnchants().size()];
					int i = 0;
					for(Entry<Enchantment, Integer> ench : bookEnchantments.getStoredEnchants().entrySet()){
						enchs[i] = ench.getKey().getId() + ":" + ench.getValue();
						i++;
					}
					actionData.enchs = enchs;
				}
			}
		}
		
		// Fireworks
		if( block_id == 402 ){
			FireworkEffectMeta fireworkMeta = (FireworkEffectMeta) item.getItemMeta();
			if( fireworkMeta.hasEffect() ){
				FireworkEffect effect = fireworkMeta.getEffect();
				if( !effect.getColors().isEmpty() ){
					int[] effectColors = new int[ effect.getColors().size() ];
					int i = 0;
					for (Color effectColor : effect.getColors()){
						effectColors[i] = effectColor.asRGB();
						i++;
					}
					actionData.effectColors = effectColors;
				}
				if( !effect.getFadeColors().isEmpty() ){
					int[] fadeColors = new int[ effect.getColors().size() ];
					int i = 0;
				    for (Color fadeColor : effect.getFadeColors()){
				    	fadeColors[i] = fadeColor.asRGB();
				    }
				    actionData.fadeColors = fadeColors;
				}
				if(effect.hasFlicker()){
					actionData.hasFlicker = true;
				}
				if(effect.hasTrail()){
					actionData.hasTrail = true;
				}
			}
		}
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setItemStackFromData();
	}
	

	/**
	 * Prism began tracking very little data about an item stack and we felt
	 * that an object wasn't necessary. That soon became a bad decision
	 * because we kept piling on data to an existing string. Now we have a lot of old
	 * data that won't work with the new object, so we must keep around the
	 * old parsing methods.
	 */
	protected void setItemStackFromData(){
		if(item == null && data != null){
			
			// New format?
			if(data.startsWith("{")){
				setItemStackFromNewDataFormat();
			} 
			
			// Old format!
			else {
				setItemStackFromOldDataFormat();
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
	protected void setItemStackFromNewDataFormat(){
		
		actionData = gson.fromJson(data, ItemStackActionData.class);
		if( actionData.block_id > 0 ){
			this.block_id = actionData.block_id;
			this.block_subid = actionData.block_subid;
		}

		item = new ItemStack(this.block_id,actionData.amt,(short)this.block_subid);
		
		// Restore enchantment
		if(actionData.enchs != null && actionData.enchs.length > 0){
			for( String ench : actionData.enchs){
				String[] enchArgs = ench.split(":");
				Enchantment enchantment = Enchantment.getById(Integer.parseInt(enchArgs[0]));
				// Restore book enchantment
				if(item.getType().equals(Material.ENCHANTED_BOOK)){
					EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
					bookEnchantments.addStoredEnchant(enchantment, Integer.parseInt(enchArgs[1]), false);
					item.setItemMeta(bookEnchantments);
				} 
				// Restore item enchantment
				else {
					item.addUnsafeEnchantment(enchantment, Integer.parseInt(enchArgs[1]));
				}
			}
		}
		
		// Leather color
		if( item.getType().name().contains("LEATHER_") && actionData.color > 0 ){
			LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
			lam.setColor(Color.fromRGB( actionData.color ));
			item.setItemMeta(lam);
		}
		// Skulls
		else if( item.getType().equals(Material.SKULL_ITEM) && actionData.owner != null ){
			SkullMeta meta = (SkullMeta) item.getItemMeta();
			meta.setOwner( actionData.owner );
			item.setItemMeta(meta);
		}
		// Written books
		else if(item.getType().equals( Material.WRITTEN_BOOK )){
	        BookMeta bookMeta = (BookMeta) item.getItemMeta();
			if(actionData.by != null && !actionData.by.isEmpty()){
				bookMeta.setAuthor( actionData.by );
				bookMeta.setTitle( actionData.title );
			}
			item.setItemMeta(bookMeta);
		}
		
		// Fireworks
		if( block_id == 402 ){
			FireworkEffectMeta fireworkMeta = (FireworkEffectMeta) item.getItemMeta();
			Builder effect = FireworkEffect.builder();
			if( actionData.effectColors != null ){
				for(int i = 0; i < actionData.effectColors.length; i++ ){
					effect.withColor( Color.fromRGB( actionData.effectColors[i] ) );
				}
				fireworkMeta.setEffect(effect.build());
			}
			if( actionData.fadeColors != null ){
				for(int i = 0; i < actionData.fadeColors.length; i++ ){
					effect.withFade( Color.fromRGB( actionData.fadeColors[i] ) );
				}
				fireworkMeta.setEffect(effect.build());
			}
			if(actionData.hasFlicker){
				effect.flicker(true);
			}
			if(actionData.hasTrail){
				effect.trail(true);
			}
			fireworkMeta.setEffect(effect.build());
			item.setItemMeta(fireworkMeta);
		}
		
		// Item display names
		if( actionData.name != null ){
			ItemMeta meta = item.getItemMeta();
			meta.setDisplayName( actionData.name );
			item.setItemMeta(meta);
		}
	}
		
		
	/**
	 * This parsing method is only for item records from prior to 1.2.
	 * @deprecated
	 */
	protected void setItemStackFromOldDataFormat(){
		
		actionData = new ItemStackActionData();
		
		String[] blockArr = data.split(":");
		if (!TypeUtils.isNumeric(blockArr[0])) return;
		
		if (blockArr.length >= 3){
			
			// Parse item/sub/quant
			this.block_id = Integer.parseInt(blockArr[0]);
			this.block_subid = Integer.parseInt(blockArr[1]);
			actionData.amt = Integer.parseInt(blockArr[2]);
			
			item = new ItemStack(this.block_id,actionData.amt,(short)this.block_subid);
			
			// Restore enchantments
			for(int i = 3; i < blockArr.length; i++){
				if(blockArr[i].contains("~")){
					if(blockArr[i].contains("color~")){
						String rgb = blockArr[i].replace("color~", "");
						if (!TypeUtils.isNumeric(rgb)) return;
						int color = Integer.parseInt(rgb);
						LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
						lam.setColor(Color.fromRGB(color));
						item.setItemMeta(lam);
					} 
					else if(blockArr[i].contains("skullowner~")){
						String owner = blockArr[i].replace("skullowner~", "");
						SkullMeta meta = (SkullMeta) item.getItemMeta();
						meta.setOwner(owner);
						item.setItemMeta(meta);
					}
					else if(blockArr[i].contains("name~")){
						String name = blockArr[i].replace("name~", "");
						ItemMeta meta = item.getItemMeta();
						meta.setDisplayName(name);
						item.setItemMeta(meta);
					}
				} else {
					if(blockArr[i].contains(";")){
						if(item.getType().name().contains("LEATHER_")){
							LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
							lam.setColor(Color.fromRGB(Integer.parseInt(blockArr[i].replaceAll(";", ""))));
							item.setItemMeta(lam);
						}
						else if(item.getType().equals(Material.SKULL_ITEM)){
							SkullMeta meta = (SkullMeta) item.getItemMeta();
							meta.setOwner(blockArr[i].replaceAll(";", ""));
							item.setItemMeta(meta);
						}
						continue;
					}
					String[] enchArgs = blockArr[i].split(",");
					Enchantment enchantment = Enchantment.getById(Integer.parseInt(enchArgs[0]));
					
					// Restore book enchantments
					if(item.getType().equals(Material.ENCHANTED_BOOK)){
						EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
						bookEnchantments.addStoredEnchant(enchantment, Integer.parseInt(enchArgs[1]), false);
						item.setItemMeta(bookEnchantments);
					} else {
					
						// Restore item enchants
						item.addUnsafeEnchantment(enchantment, Integer.parseInt(enchArgs[1]));
						
					}
				}
			}
		}
	}
	
	
	/**
	 * 
	 */
	public ItemStackActionData getActionData(){
		return this.actionData;
	}

	
	/**
	 * 
	 * @return
	 */
	public ItemStack getItem(){
		return item;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		if(item != null){
			String fullItemName = ItemUtils.getItemFullNiceName(item, this.materialAliases);
			name = actionData.amt + " " + fullItemName;
		}
		return name;
	}
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback( Player player, QueryParameters parameters, boolean is_preview ){
		return placeItems( player, parameters, is_preview );
	}
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyRestore( Player player, QueryParameters parameters, boolean is_preview ){
		return placeItems( player, parameters, is_preview );
	}
	
	
	
	/**
	 * 
	 * @return
	 */
	protected ChangeResult placeItems( Player player, QueryParameters parameters, boolean is_preview ){
		
		ChangeResultType result = null;
		
		if( is_preview ){
			return new ChangeResult( ChangeResultType.PLANNED, null );
		}
		
		if( plugin.getConfig().getBoolean("prism.appliers.allow_rollback_items_removed_from_container") ){
			
			Block block = getWorld().getBlockAt( getLoc() );
			InventoryHolder container = null;
			if(block.getType().equals(Material.CHEST)){
				container = (Chest) block.getState();
			}
			else if( block.getType().equals(Material.DISPENSER) ){
				container = (Dispenser) block.getState();
			}
			else if( block.getType().equals(Material.FURNACE) ){
				container = (Furnace) block.getState();
			}
			else if( block.getType().equals(Material.BREWING_STAND) ){
				container = (BrewingStand) block.getState();
			}
			else if( block.getType().equals(Material.DROPPER) ){
				container = (Dropper) block.getState();
			}
			else if( block.getType().equals(Material.HOPPER) ){
				container = (Hopper) block.getState();
			}
			
			if(container != null){
				
				Inventory inv = container.getInventory();
				
				// Rolling back a:remove should place the item into the inventory
				// Restoring a:insert should place the item into the inventory
				if( (parameters.getProcessType().equals(PrismProcessType.ROLLBACK) && getType().getName().equals("item-remove"))
					|| (parameters.getProcessType().equals(PrismProcessType.RESTORE) && getType().getName().equals("item-insert")) ){
					
					boolean added = false;
					
					// We'll attempt to put it back in the same slot
					if( getActionData().slot >= 0 ){
						// Ensure slot exists in this inventory
						// I'm not sure why this happens but sometimes
						// a slot larger than the contents size is recorded
						// and triggers ArrayIndexOutOfBounds
						// https://snowy-evening.com/botsko/prism/450/
						if( getActionData().slot < inv.getSize() ){
							ItemStack currentSlotItem = inv.getItem( getActionData().slot );
							// Make sure nothing's there.
							if( currentSlotItem == null ){
								result = ChangeResultType.APPLIED;
								added = true;
								inv.setItem(getActionData().slot, getItem());
							}
						}
					}
					// If that failed we'll attempt to put it anywhere
					if( !added ){
						HashMap<Integer,ItemStack> leftovers = InventoryUtils.addItemToInventory(container.getInventory(), getItem());
						if(leftovers.size() > 0){
							result = ChangeResultType.SKIPPED;
//							plugin.debug("Item placement into container skipped because container was full.");
						} else {
							result = ChangeResultType.APPLIED;
						}
					}
				}
				
				// Rolling back a:insert should remove the item from the inventory
				// Restoring a:remove should remove the item from the inventory
				if( (parameters.getProcessType().equals(PrismProcessType.ROLLBACK) && getType().getName().equals("item-insert"))
					|| (parameters.getProcessType().equals(PrismProcessType.RESTORE) && getType().getName().equals("item-remove")) ){
						
					// does inventory have item?
					boolean removed = false;
					
					// We'll attempt to take it from the same slot
					if( getActionData().slot >= 0 ){
						ItemStack currentSlotItem = inv.getItem( getActionData().slot );
						// Make sure something's there.
						if( currentSlotItem != null ){
							currentSlotItem.setAmount( currentSlotItem.getAmount() - getItem().getAmount() );
							result = ChangeResultType.APPLIED;
							removed = true;
							inv.setItem(getActionData().slot, currentSlotItem);
						}
					}
					// If that failed we'll attempt to take it from anywhere
					if( !removed ){
						int slot = InventoryUtils.inventoryHasItem( container.getInventory(), getItem().getTypeId(), getItem().getDurability());
						if(slot > -1){
							container.getInventory().removeItem(getItem());
							result = ChangeResultType.APPLIED;
						} else {
//							plugin.debug("Item removal from container skipped because it's not inside.");
							result = ChangeResultType.SKIPPED;
						}
					}
				}
			}
		}
		return new ChangeResult( result, null );
	}
}