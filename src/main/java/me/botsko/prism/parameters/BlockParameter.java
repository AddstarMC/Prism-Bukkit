package me.botsko.prism.parameters;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.bukkit.command.CommandSender;

import me.botsko.elixr.ItemUtils;
import me.botsko.elixr.MaterialAliases;
import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

public class BlockParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 * @return
	 */
	public String getName(){
		return "Block";
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String[] getHelp(){
		return new String[]{};
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Pattern getArgumentPattern(){
		return Pattern.compile("(b):([\\w,:]+)");
	}
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, Matcher input, CommandSender sender ){
		String[] blocks = input.group(2).split(",");
		
		if(blocks.length > 0){
			for(String b : blocks){
		
				// if user provided id:subid
				if(b.contains(":") && b.length() >= 3){
					String[] ids = b.split(":");
					if(ids.length == 2 && TypeUtils.isNumeric(ids[0]) && TypeUtils.isNumeric(ids[1])){
						query.addBlockFilter( Integer.parseInt( ids[0] ), Byte.parseByte( ids[1] ) );
					} else {
						throw new IllegalArgumentException("Invalid block name '"+b+"'. Try /pr ? for help");
					}
				} else {
					
					// It's id without a subid
					if(TypeUtils.isNumeric(b)){
						query.addBlockFilter( Integer.parseInt(b), (byte)0 );
					} else {
						
						// Lookup the item name, get the ids
						MaterialAliases items = Prism.getItems();
						ArrayList<int[]> itemIds = items.getIdsByAlias( b );
						if(itemIds.size() > 0){
							for(int[] ids : itemIds){
								if(ids.length == 2){
									// If we really care about the sub id because it's a whole different item
									if(ItemUtils.dataValueUsedForSubitems(ids[0])){
										query.addBlockFilter( ids[0], (byte) ids[1] );
									} else {
										query.addBlockFilter( ids[0], (byte)0 );
									}
								}
							}
						} else {
							throw new IllegalArgumentException("Invalid block name '"+b+"'. Try /pr ? for help");
						}
					}
				}
			}
		}
	}
	
	
	/**
	 * 
	 */
	public void defaultTo( QueryParameters query, CommandSender sender ){
		return;
	}
}