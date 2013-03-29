package me.botsko.prism.actions;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.Location;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.Hanging;
import org.bukkit.entity.ItemFrame;
import org.bukkit.entity.Painting;
import org.bukkit.entity.Player;

public class HangingItemAction extends GenericAction {
	
	public class HangingItemActionData {
		public String type;
		public String direction;
	}
	
	/**
	 * 
	 */
	protected HangingItemActionData actionData;
	

	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public void setHanging( Hanging hanging ){
		
		actionData = new HangingItemActionData();
		
		if(hanging != null){
			this.actionData.type = hanging.getType().name().toLowerCase();
			this.actionData.direction = hanging.getAttachedFace().name().toLowerCase();
			this.world_name = hanging.getWorld().getName();
			this.x = hanging.getLocation().getBlockX();
			this.y = hanging.getLocation().getBlockY();
			this.z = hanging.getLocation().getBlockZ();
		}
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		if(data != null){
			actionData = gson.fromJson(data, HangingItemActionData.class);
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
	 * @return
	 */
	public String getHangingType(){
		return this.actionData.type;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public BlockFace getDirection(){
		if(actionData.direction != null){
			return BlockFace.valueOf(actionData.direction.toUpperCase());
		}
		return null;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "hangingitem";
		name = data.toLowerCase();
		if(this.actionData.type != null){
			name = this.actionData.type;
		}
		return name;
	}
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback( Player player, QueryParameters parameters, boolean is_preview ){
		return hangItem( player, parameters, is_preview );
	}
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyRestore( Player player, QueryParameters parameters, boolean is_preview ){
		return hangItem( player, parameters, is_preview );
	}
	
	
	/**
	 * 
	 */
	public ChangeResult hangItem( Player player, QueryParameters parameters, boolean is_preview ){
		
		BlockFace attachedFace = getDirection().getOppositeFace();

		Location loc = new Location( getWorld(), getX(), getY(), getZ()).getBlock().getRelative(getDirection()).getLocation();
		
		// bug filed:
		// https://bukkit.atlassian.net/browse/BUKKIT-3371
		if( getHangingType().equals("item_frame") ){
			if(!BlockUtils.materialMeansBlockDetachment( loc.getBlock().getType() )){
				Hanging hangingItem = getWorld().spawn(loc, ItemFrame.class);
				hangingItem.setFacingDirection( attachedFace, true );
				return new ChangeResult( ChangeResultType.APPLIED, null );
			}
		}
		else if( getHangingType().equals("painting") ){
			if(!BlockUtils.materialMeansBlockDetachment( loc.getBlock().getType() )){
				Hanging hangingItem = getWorld().spawn(loc, Painting.class);
				hangingItem.setFacingDirection( getDirection(), true );
				return new ChangeResult( ChangeResultType.APPLIED, null );
			}
		}
		return new ChangeResult( ChangeResultType.SKIPPED, null );
	}
}