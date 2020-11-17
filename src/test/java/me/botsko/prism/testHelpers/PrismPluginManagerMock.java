package me.botsko.prism.testHelpers;

import be.seeseemelk.mockbukkit.ServerMock;
import be.seeseemelk.mockbukkit.plugin.PluginManagerMock;
import org.bukkit.event.Event;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.plugin.EventExecutor;
import org.bukkit.plugin.Plugin;

import java.util.ArrayList;
import java.util.List;

/**
 * Created for the Prism-Bukkit Project.
 * Created by Narimm on 19/11/2020.
 */
public class PrismPluginManagerMock extends PluginManagerMock {
    List<Class<? extends Event>> customEventList = new ArrayList<>();
    public PrismPluginManagerMock(ServerMock server) {
        super(server);
    }

    @Override
    public void registerEvent(Class<? extends Event> event, Listener listener, EventPriority priority, EventExecutor executor, Plugin plugin) {
        customEventList.add(event);
        registerEvents(listener,plugin);
        super.registerEvent(event, listener, priority, executor, plugin);
    }
}
