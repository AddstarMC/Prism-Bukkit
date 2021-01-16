package me.botsko.prism.appliers;

import me.botsko.prism.api.objects.ApplierResult;
import org.bukkit.command.CommandSender;

public interface ApplierCallback {
    void handle(CommandSender sender, ApplierResult result);
}
