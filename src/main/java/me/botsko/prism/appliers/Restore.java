package me.botsko.prism.appliers;

import java.util.List;

import org.bukkit.command.CommandSender;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Handler;

public class Restore extends Preview {

    /**
     * 
     * @param plugin
     * @return
     */
    public Restore(Prism plugin, CommandSender sender, List<Handler> results, QueryParameters parameters,
            ApplierCallback callback) {
        super( plugin, sender, results, parameters, callback );
    }

    /**
     * Set preview move and then do a rollback
     * 
     * @return
     */
    @Override
    public void preview() {
        is_preview = true;
        apply();
    }
}