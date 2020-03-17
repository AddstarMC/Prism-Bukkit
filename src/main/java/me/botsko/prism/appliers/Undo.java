package me.botsko.prism.appliers;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Handler;
import org.bukkit.entity.Player;

import java.util.List;

public class Undo extends Preview {

    /**
     * @param plugin
     * @return
     */
    public Undo(Prism plugin, Player player, List<Handler> results, QueryParameters parameters,
                ApplierCallback callback) {
        super(plugin, player, results, parameters, callback);
    }

    /**
     * Set preview move and then do a rollback
     *
     * @return
     */
    @Override
    public void preview() {
        player.sendMessage(Prism.messenger.playerError("You can't preview an undo."));
    }
}