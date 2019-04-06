package me.botsko.prism.actions;

import org.bukkit.block.Block;

import me.botsko.prism.Prism;

public class BlockShiftAction extends GenericAction {

	public class BlockShiftActionData {
		public int x;
		public int y;
		public int z;
	}

	/**
	 * 
	 */
	protected BlockShiftActionData actionData;

	/**
	 * 
	 * @param from
	 */
	public void setBlock(Block from) {

		// Build an object for the specific details of this action
		actionData = new BlockShiftActionData();

		// Store information for the action
		if (from != null) {

			setMaterial(from.getType());
			setBlockData(from.getBlockData());
			actionData.x = from.getX();
			actionData.y = from.getY();
			actionData.z = from.getZ();

		}
	}

	@Override
	public boolean hasExtraData() {
		return actionData != null;
	}

	@Override
	public String serialize() {
		return gson().toJson(actionData);
	}
	
	@Override
	public void deserialize(String data) {
		if (data != null && data.startsWith("{")) {
			actionData = gson().fromJson(data, BlockShiftActionData.class);
		}
	}

	/**
	 * 
	 * @return
	 */
	@Override
	public String getNiceName() {
		String location = "unknown";
		if(actionData != null) {
			location = actionData.x + " " + actionData.y + " " + actionData.z;
		}
		
		return Prism.getItems().getAlias(getMaterial(), getBlockData()) + " from " + location;
	}
}