package me.botsko.prism.actionlibs;

import java.util.Map;

import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.BlockChangeAction;
import me.botsko.prism.actions.BlockShiftAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.EntityTravelAction;
import me.botsko.prism.actions.GrowAction;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.actions.HangingItemAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.PlayerAction;
import me.botsko.prism.actions.PlayerDeathAction;
import me.botsko.prism.actions.PrismProcessAction;
import me.botsko.prism.actions.PrismRollbackAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.actions.UseAction;
import me.botsko.prism.actions.VehicleAction;
import me.botsko.prism.appliers.PrismProcessType;

import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Hanging;
import org.bukkit.entity.Player;
import org.bukkit.entity.Vehicle;
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause;
import org.bukkit.inventory.ItemStack;

public class ActionFactory {
	

	/**
	 * GenericAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, String player ){
		BlockAction a = new BlockAction();
		a.setActionType(action_type);
		a.setPlayerName(player);
		return a;
	}

	
	/**
	 * BlockAction
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public static Handler create( String action_type, Block block, String player ){
		BlockAction a = new BlockAction();
		a.setActionType(action_type);
		a.setBlock(block);
		a.setPlayerName(player);
		return a;
	}
	
	
	/**
	 * BlockChangeAction | WorldeditAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, Location loc, int oldId, byte oldSubid, int newId, byte newSubid, String player ){
		BlockChangeAction a = new BlockChangeAction();
		a.setActionType(action_type);
		a.setBlockId(newId);
		a.setBlockSubId(newSubid);
		a.setOldBlockId(oldId);
		a.setOldBlockSubId(oldSubid);
		a.setPlayerName(player);
		a.setLoc(loc);
		return a;
	}
	
	
	/**
	 * BlockShiftAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, Block from, Location to, String player ){
		BlockShiftAction a = new BlockShiftAction();
		a.setActionType(action_type);
		a.setBlock(from);
		a.setPlayerName(player);
		a.setToLocation(to);
		return a;
	}
	
	
	/**
	 * EntityAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create(String action_type, Entity entity, String player ){
		return ActionFactory.create(action_type, entity, player, null);
	}
	public static Handler create(String action_type, Entity entity, String player, String dyeUsed ){
		EntityAction a = new EntityAction();
		a.setActionType(action_type);
		a.setPlayerName(player);
		a.setEntity(entity,dyeUsed);
		return a;
	}
	
	
	/**
	 * EntityTravelAction
	 * @param action_type
	 */
	public static Handler create( String action_type, Entity entity, Location from, Location to, TeleportCause cause ){
		EntityTravelAction a = new EntityTravelAction();
		a.setEntity(entity);
		a.setActionType(action_type);
		a.setLoc(from);
		a.setToLocation(to);
		return a;
	}
	
	
	/**
	 * GrowAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, BlockState blockstate, String player ){
		GrowAction a = new GrowAction();
		a.setActionType(action_type);
		a.setBlock(blockstate);
		a.setPlayerName(player);
		return a;
	}
	
	
	/**
	 * HangingItemAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, Hanging hanging, String player ){
		HangingItemAction a = new HangingItemAction();
		a.setActionType(action_type);
		a.setHanging(hanging);
		a.setPlayerName(player);
		return a;
	}
	
	
	/**
	 * ItemStackAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, ItemStack item, Map<Enchantment,Integer> enchantments, Location loc, String player ){
		return ActionFactory.create(action_type,item,1,-1,enchantments,loc,player);
	}
	public static Handler create( String action_type, ItemStack item, int quantity, int slot, Map<Enchantment,Integer> enchantments, Location loc, String player ){
		ItemStackAction a = new ItemStackAction();
		a.setActionType(action_type);
		a.setLoc(loc);
		a.setPlayerName(player);
		a.setItem(item, quantity, slot, enchantments);
		return a;
	}
	
	
	/**
	 * PlayerAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, Player player, String additionalInfo ){
		PlayerAction a = new PlayerAction();
		a.setActionType(action_type);
		a.setPlayerName(player);
		a.setLoc( player.getLocation() );
		a.setData(additionalInfo);
		return a;
	}
	
	
	/**
	 * PlayerDeathAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, Player player, String cause, String attacker ){
		PlayerDeathAction a = new PlayerDeathAction();
		a.setActionType(action_type);
		a.setPlayerName(player);
		a.setLoc( player.getLocation() );
		a.setCause(cause);
		a.setAttacker(attacker);
		return a;
	}
	
	
	/**
	 * PrismProcessActionData
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, PrismProcessType processType, Player player, String parameters ){
		PrismProcessAction a = new PrismProcessAction();
		a.setActionType(action_type);
		a.setPlayerName(player);
		a.setLoc( player.getLocation() );
		a.setProcessData(processType,parameters);
		return a;
	}
	
	
	/**
	 * PrismRollbackAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, BlockState oldblock, BlockState newBlock, String player, int parent_id ){
		PrismRollbackAction a = new PrismRollbackAction();
		a.setActionType(action_type);
		a.setPlayerName(player);
		a.setLoc(oldblock.getLocation());
		a.setBlockChange(oldblock, newBlock, parent_id);
		return a;
	}
	
	
	/**
	 * SignAction
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public static Handler create( String action_type, Block block, String[] lines, String player ){
		SignAction a = new SignAction();
		a.setActionType(action_type);
		a.setPlayerName(player);
		a.setBlock( block, lines );
		return a;
	}
	
	
	/**
	 * UseAction
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public static Handler create( String action_type, String item_used, Block block, String player ){
		UseAction a = new UseAction();
		a.setActionType(action_type);
		a.setPlayerName(player);
		a.setLoc( block.getLocation() );
		a.setData(item_used);
		return a;
	}
	
	
	/**
	 * VehicleAction
	 * @param action_type
	 * @param player
	 */
	public static Handler create( String action_type, Vehicle vehicle, String player ){
		VehicleAction a = new VehicleAction();
		a.setActionType(action_type);
		a.setPlayerName(player);
		a.setLoc( vehicle.getLocation() );
		a.setVehicle(vehicle);
		return a;
	}
}