package me.botsko.prism.actions;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.helion3.prism.libs.elixr.InventoryUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.appliers.PrismProcessType;

import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.MiscUtils;
import org.bukkit.*;
import org.bukkit.Bukkit;
import org.bukkit.Color;
import org.bukkit.DyeColor;
import org.bukkit.FireworkEffect;
import org.bukkit.FireworkEffect.Builder;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.Jukebox;
import org.bukkit.block.banner.Pattern;
import org.bukkit.block.banner.PatternType;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Item;
import org.bukkit.entity.ItemFrame;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.BannerMeta;
import org.bukkit.inventory.meta.BookMeta;
import org.bukkit.inventory.meta.EnchantmentStorageMeta;
import org.bukkit.inventory.meta.FireworkEffectMeta;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.LeatherArmorMeta;
import org.bukkit.inventory.meta.SkullMeta;

public class ItemStackAction extends GenericAction {

    /** @deprecated For legacy data pre-1.11.2-2.0.9 */
    public class ItemStackActionData {
        // Native meta
        public int amt;
        public int slot = -1;
        public String meta;

        // Legacy
        public int color;
        public String name;
        public String owner;
        public String[] enchs;
        public String by;
        public String title;
        public String[] lore;
        public String[] content;
        public int[] effectColors;
        public int[] fadeColors;
        public boolean hasFlicker;
        public boolean hasTrail;
        public String[] patterns;
        public String baseColor;
    }

    /** Tempoarily a separate class for serialization, until we can get rid of legacy */
    public class ItemStackNewActionData {
        public int    amt;
        public int    slot = -1;
        public String meta;
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
     * We store the enchantments here because an event like item enchant doesn't
     * give us the item with the enchantments already on it.
     */
    protected Map<Enchantment, Integer> enchantments;

    /**
     * 
     * @param item the item
     * @param quantity qty
     * @param enchantments enchants
     * @param slot  eq slot
     */
    /** Sets and serializes the ItemStack of this action */
    public void setItem(ItemStack item, int quantity, int slot, Map<Enchantment, Integer> enchantments) {
        actionData = new ItemStackActionData();

        if( item == null || item.getAmount() <= 0 ) {
            this.setCanceled( true );
            return;
        }

        // Set basics
        this.item = item;
        this.block_id = item.getTypeId();
        this.block_subid = item.getDurability();
        actionData.amt = quantity;

        if (slot >= 0)
            actionData.slot = slot;

        // Necessary because enchant-item event doesn't directly apply the new enchants
        if (enchantments != null)
            this.item.addEnchantments(enchantments);

        if ( item.hasItemMeta() )
        {
            // Serialize the ItemStack's meta using native API
            actionData.meta = MiscUtils.serializeToBase64( item.getItemMeta() );

            if (actionData.meta == null)
                Prism.debug("Could not base64 serialize item meta: " + item);
        }
    }

    /**
     *
     */
    @Override
    public void save() {
        if (actionData == null)
            return;

        ItemStackNewActionData toSerialize = new ItemStackNewActionData();

        toSerialize.amt  = actionData.amt;
        toSerialize.slot = actionData.slot;
        toSerialize.meta = actionData.meta;

        data = gson.toJson(toSerialize);
    }

    /**
	 * 
	 */
    @Override
    public void setData(String data) {
        this.data = data;

        if ( item != null || data == null )
            return;

        if ( data.contains("\"hasFlicker\"") )
            setItemStackFromLegacyDataFormat();
        else
            setItemStackFromNativeDataFormat();
    }

    protected void setItemStackFromNativeDataFormat() {
        if( data == null || !data.startsWith( "{" ) )
            return;

        actionData = gson.fromJson(data, ItemStackActionData.class);
        item = new ItemStack( this.block_id, actionData.amt, (short) this.block_subid );

        // Deserialize any metadata
        if (actionData.meta != null)
        {
            ItemMeta meta = MiscUtils.deserializeFromBase64(actionData.meta);

            if (meta == null)
                Prism.debug("Could not base64 deserialize item meta: " + item);
            else
                item.setItemMeta(meta);
        }
    }

    /**
     * Converts JSON data from pre-1.11.2-2.0.9 to item stack
     * @deprecated Future item events will reliably serialize data using Bukkit's API
     */
    protected void setItemStackFromLegacyDataFormat() {

        if( data == null || !data.startsWith( "{" ) )
            return;

        actionData = gson.fromJson( data, ItemStackActionData.class );

        item = new ItemStack( this.block_id, actionData.amt, (short) this.block_subid );

        // Restore enchantment
        if( actionData.enchs != null && actionData.enchs.length > 0 ) {
            for ( final String ench : actionData.enchs ) {
                final String[] enchArgs = ench.split( ":" );
                final Enchantment enchantment = Enchantment.getById( Integer.parseInt( enchArgs[0] ) );
                // Restore book enchantment
                if( item.getType().equals( Material.ENCHANTED_BOOK ) ) {
                    final EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
                    bookEnchantments.addStoredEnchant( enchantment, Integer.parseInt( enchArgs[1] ), false );
                    item.setItemMeta( bookEnchantments );
                }
                // Restore item enchantment
                else {
                    item.addUnsafeEnchantment( enchantment, Integer.parseInt( enchArgs[1] ) );
                }
            }
        }

        // Leather color
        if( item.getType().name().contains( "LEATHER_" ) && actionData.color > 0 ) {
            final LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
            lam.setColor( Color.fromRGB( actionData.color ) );
            item.setItemMeta( lam );
        }
        // Skulls
        else if( item.getType().equals( Material.SKULL_ITEM ) && actionData.owner != null ) {
            final SkullMeta meta = (SkullMeta) item.getItemMeta();
            meta.setOwner( actionData.owner );
            item.setItemMeta( meta );
        }
        // Written books
        else if( item.getItemMeta() instanceof BookMeta ) {
            final BookMeta bookMeta = (BookMeta) item.getItemMeta();
            bookMeta.setAuthor( actionData.by );
            bookMeta.setTitle( actionData.title );
            bookMeta.setPages( actionData.content );
            item.setItemMeta( bookMeta );
        }

        // Fireworks
        if( block_id == 402 && actionData.effectColors != null && actionData.effectColors.length > 0 ) {
            final FireworkEffectMeta fireworkMeta = (FireworkEffectMeta) item.getItemMeta();
            final Builder effect = FireworkEffect.builder();
            if( actionData.effectColors != null ) {
                for ( int i = 0; i < actionData.effectColors.length; i++ ) {
                    effect.withColor( Color.fromRGB( actionData.effectColors[i] ) );
                }
                fireworkMeta.setEffect( effect.build() );
            }
            if( actionData.fadeColors != null ) {
                for ( int i = 0; i < actionData.fadeColors.length; i++ ) {
                    effect.withFade( Color.fromRGB( actionData.fadeColors[i] ) );
                }
                fireworkMeta.setEffect( effect.build() );
            }
            if( actionData.hasFlicker ) {
                effect.flicker( true );
            }
            if( actionData.hasTrail ) {
                effect.trail( true );
            }
            fireworkMeta.setEffect( effect.build() );
            item.setItemMeta( fireworkMeta );
        }

        // Banners
        if( block_id == 425 && actionData.patterns != null ) {
        	final BannerMeta bannerMeta = (BannerMeta) item.getItemMeta();
        	if (actionData.baseColor != null) {
        		bannerMeta.setBaseColor(DyeColor.valueOf(actionData.baseColor));
        	}

        	for (String pattern : actionData.patterns) {
        		String[] parts = pattern.split(":");
        		bannerMeta.addPattern(new Pattern(DyeColor.valueOf(parts[0]), PatternType.valueOf(parts[1])));
        	}
        	item.setItemMeta(bannerMeta);
        }

        // Item display names
        final ItemMeta meta = item.getItemMeta();
        if( actionData.name != null ) {
            meta.setDisplayName( actionData.name );
        }
        if( actionData.lore != null ) {
            meta.setLore( Arrays.asList( actionData.lore ) );
        }
        item.setItemMeta( meta );
    }

    /**
	 * 
	 */
    public ItemStackActionData getActionData() {
        return this.actionData;
    }

    /**
     * 
     * @return
     */
    public ItemStack getItem() {
        return item;
    }

    /**
     * 
     * @return
     */
    @Override
    public String getNiceName() {
        String name = "";
        if( item != null ) {
            final String fullItemName = com.helion3.prism.libs.elixr.ItemUtils.getItemFullNiceName( item );
            name = actionData.amt + " " + fullItemName;
        }
        return name;
    }

    /**
	 * 
	 */
    @Override
    public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean is_preview) {
        return placeItems( player, parameters, is_preview );
    }

    /**
	 * 
	 */
    @Override
    public ChangeResult applyRestore(Player player, QueryParameters parameters, boolean is_preview) {
        return placeItems( player, parameters, is_preview );
    }

    /**
     * 
     * @return
     */
    protected ChangeResult placeItems(Player player, QueryParameters parameters, boolean is_preview) {

        ChangeResultType result = null;

        if( is_preview ) { return new ChangeResult( ChangeResultType.PLANNED, null ); }

        if( plugin.getConfig().getBoolean( "prism.appliers.allow-rollback-items-removed-from-container" ) ) {

            final Block block = getWorld().getBlockAt( getLoc() );
            Inventory inventory = null;

            // Item drop/pickup from player inventories
            if( getType().getName().equals( "item-drop" ) || getType().getName().equals( "item-pickup" ) ) {

                // Is player online?
                final String playerName = getPlayerName();
                final Player onlinePlayer = Bukkit.getServer().getPlayer( playerName );
                if( onlinePlayer != null ) {
                    inventory = onlinePlayer.getInventory();
                } else {
                    // Skip if the player isn't online
                    Prism.debug( "Skipping inventory process because player is offline" );
                    return new ChangeResult( ChangeResultType.SKIPPED, null );
                }
            } else {
                if( block.getType().equals( Material.JUKEBOX ) ) {
                    final Jukebox jukebox = (Jukebox) block.getState();
                    jukebox.setPlaying( item.getType() );
                    jukebox.update();
                } else if( block.getState() instanceof InventoryHolder ) {
                    final InventoryHolder ih = (InventoryHolder) block.getState();
                    inventory = ih.getInventory();
                } else {
                    for (Entity e : BlockUtils.findHangingEntities( getLoc() )) {
                        if( !e.getType().equals( EntityType.ITEM_FRAME ) ) continue;
                        // Some modded servers seems to list entities in the chunk
                        // that exists in other worlds. No idea why but we can at
                        // least check for it.
                        // https://snowy-evening.com/botsko/prism/318/
                        if( !block.getWorld().equals( e.getWorld() ) ) continue;
                        final ItemFrame frame = (ItemFrame) e;
                        // When rolling back a:remove or restoring a:insert we should place the item into frame
                        if( (getType().getName().equals( "item-remove" ) && parameters.getProcessType().equals( PrismProcessType.ROLLBACK )) ||
                            (getType().getName().equals( "item-insert" ) && parameters.getProcessType().equals( PrismProcessType.RESTORE ))  ){
                            // Only place if item frame is empty
                            if (frame.getItem().getType().equals(Material.AIR)) {
                                frame.setItem(item);
                                result = ChangeResultType.APPLIED;
                                break;
                            }
                        // When rolling back a:insert or restoring a:remove we should remove the item from frame
                        } else {
                            // Only remove if item frame has what was placed
                            if (frame.getItem().equals(item)) {
                                frame.setItem(null);
                                result = ChangeResultType.APPLIED;
                                break;
                            }
                        }
                    }
                }
            }

            if( inventory != null ) {

                final PrismProcessType pt = parameters.getProcessType();
                final String n = getType().getName();

                // Rolling back a:remove or a:drop should place the item into
                // the inventory
                // Restoring a:insert or a:pickup should place the item into the
                // inventory
                if( ( pt.equals( PrismProcessType.ROLLBACK ) && ( n.equals( "item-remove" ) || n.equals( "item-drop" ) ) )
                        || ( pt.equals( PrismProcessType.RESTORE ) && ( n.equals( "item-insert" ) || n
                                .equals( "item-pickup" ) ) ) ) {

                    boolean added = false;

                    // We'll attempt to put it back in the same slot
                    if( actionData.slot >= 0 ) {
                        // Ensure slot exists in this inventory
                        // I'm not sure why this happens but sometimes
                        // a slot larger than the contents size is recorded
                        // and triggers ArrayIndexOutOfBounds
                        // https://snowy-evening.com/botsko/prism/450/
                        if( actionData.slot < inventory.getSize() ) {
                            final ItemStack currentSlotItem = inventory.getItem( actionData.slot );
                            // Make sure nothing's there.
                            if( currentSlotItem == null ) {
                                result = ChangeResultType.APPLIED;
                                added = true;
                                inventory.setItem( actionData.slot, getItem() );
                            }
                        }
                    }
                    // If that failed we'll attempt to put it anywhere
                    if( !added ) {
                        final HashMap<Integer, ItemStack> leftovers = InventoryUtils.addItemToInventory( inventory,
                                getItem() );
                        if( leftovers.size() > 0 ) {
                            Prism.debug( "Skipping adding items because there are leftovers" );
                            result = ChangeResultType.SKIPPED;
                        } else {
                            result = ChangeResultType.APPLIED;
                            added = true;
                        }
                    }

                    // Item was added to the inv, we need to remove the entity
                    if( added && ( n.equals( "item-drop" ) || n.equals( "item-pickup" ) ) ) {
                        final Entity[] entities = getLoc().getChunk().getEntities();
                        for ( final Entity entity : entities ) {
                            if( entity instanceof Item ) {
                                final ItemStack stack = ( (Item) entity ).getItemStack();
                                if( stack.isSimilar( getItem() ) ) {
                                    // Remove the event's number of items from
                                    // the stack
                                    stack.setAmount( stack.getAmount() - getItem().getAmount() );
                                    if( stack.getAmount() == 0 ) {
                                        entity.remove();
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }

                // Rolling back a:insert or a:pickup should remove the item from
                // the inventory
                // Restoring a:remove or a:drop should remove the item from the
                // inventory
                if( ( pt.equals( PrismProcessType.ROLLBACK ) && ( n.equals( "item-insert" ) || n.equals( "item-pickup" ) ) )
                        || ( pt.equals( PrismProcessType.RESTORE ) && ( n.equals( "item-remove" ) || n
                                .equals( "item-drop" ) ) ) ) {

                    // does inventory have item?
                    boolean removed = false;

                    // We'll attempt to take it from the same slot
                    if( actionData.slot >= 0 ) {

                        if( actionData.slot > inventory.getContents().length ) {
                            inventory.addItem( getItem() );
                        } else {
                            final ItemStack currentSlotItem = inventory.getItem( actionData.slot );
                            // Make sure something's there.
                            if( currentSlotItem != null ) {
                                currentSlotItem.setAmount( currentSlotItem.getAmount() - getItem().getAmount() );
                                result = ChangeResultType.APPLIED;
                                removed = true;
                                inventory.setItem( actionData.slot, currentSlotItem );
                            }
                        }
                    }
                    // If that failed we'll attempt to take it from anywhere
                    if( !removed ) {
                        final int slot = InventoryUtils.inventoryHasItem( inventory, getItem().getTypeId(), getItem()
                                .getDurability() );
                        if( slot > -1 ) {
                            inventory.removeItem( getItem() );
                            result = ChangeResultType.APPLIED;
                            removed = true;
                        } else {
                            Prism.debug( "Item removal from container skipped because it's not currently inside." );
                            result = ChangeResultType.SKIPPED;
                        }
                    }

                    // If the item was removed and it's a drop type, re-drop it
                    if( removed && ( n.equals( "item-drop" ) || n.equals( "item-pickup" ) ) ) {
                        com.helion3.prism.libs.elixr.ItemUtils.dropItem( getLoc(), getItem() );
                    }
                }
            }
        }
        return new ChangeResult( result, null );
    }
}