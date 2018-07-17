package me.botsko.prism.parameters;

import me.botsko.prism.utils.ItemUtils;
import me.botsko.prism.utils.MaterialAliases;
import me.botsko.prism.utils.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.material.MaterialData;

import java.util.ArrayList;
import java.util.regex.Pattern;

public class BlockParameter extends SimplePrismParameterHandler {

	/**
	 * 
	 */
	public BlockParameter() {
		super("Block", Pattern.compile("[\\w,:]+"), "b");
	}

	/**
	 * 
	 */
	@Override
	public void process(QueryParameters query, String alias, String input, CommandSender sender) {
		final String[] blocks = input.split(",");

		if (blocks.length > 0) {
			for (final String b : blocks) {

				// if user provided id:subid
				if (b.contains(":") && b.length() >= 3) {
					final String[] ids = b.split(":");
					Material mat = Material.matchMaterial(ids[0]);
					if (ids.length == 2 && mat != null && TypeUtils.isNumeric(ids[1])) {
						query.addBlockFilter(mat, Short.parseShort(ids[1]));
					} else {
						throw new IllegalArgumentException("Invalid block name '" + b + "'. Try /pr ? for help");
					}
				} else {

					// It's id without a subid
					// TODO: This goes away entirely
					if (TypeUtils.isNumeric(b)) {
						query.addBlockFilter(Material.matchMaterial(b), (short) 0);
					} else {
						Material mat = Material.matchMaterial(b);
						if(mat != null)
							query.addBlockFilter(mat, (short)0);
						
						else {
							// Lookup the item name, get the ids
							final MaterialAliases items = Prism.getItems();
							final ArrayList<MaterialData> itemMaterials = items.getMaterialsByAlias(b);
	
							if (itemMaterials.size() > 0) {
								for (final MaterialData data : itemMaterials) {
									// If we really care about the sub id
									// because it's a whole different item
									// TODO: 1.13
									short d = data.getData();
	
									if (ItemUtils.dataValueUsedForSubitems(data.getItemType())) {
										query.addBlockFilter(data.getItemType(), d);
									} else {
										query.addBlockFilter(data.getItemType(), (short) 0);
									}
								}
							} else {
								throw new IllegalArgumentException("Invalid block name '" + b + "'. Try /pr ? for help");
							}
						}
					}
				}
			}
		}
	}
}