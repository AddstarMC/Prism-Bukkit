package me.botsko.prism.utils;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.wands.Wand;

public class WandUtils {

	public static boolean playerUsesWandOnClick(Player player, Location loc) {

		if (Prism.playersWithActiveTools.containsKey(player.getName())) {

			final Wand wand = Prism.playersWithActiveTools.get(player.getName());

			if (wand == null)
				return false;

			final Material item = wand.getItem();
			if (player.getInventory().getItemInMainHand().getType() == item) {
				// Left click is for current location
				wand.playerLeftClick(player, loc);
				return true;
			}
		}

		return false;

	}

}