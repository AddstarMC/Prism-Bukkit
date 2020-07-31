package me.botsko.prism.commandlibs;

import me.botsko.prism.Prism;
import net.kyori.adventure.platform.bukkit.BukkitAudiences;

import java.util.List;

public interface SubHandler {
    void handle(CallInfo call);

    List<String> handleComplete(CallInfo call);
}