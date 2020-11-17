package me.botsko.prism.listeners;

import be.seeseemelk.mockbukkit.MockBukkit;
import be.seeseemelk.mockbukkit.MockPlugin;
import be.seeseemelk.mockbukkit.ServerMock;
import me.botsko.prism.Prism;
import org.bukkit.Server;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Created for the Prism-Bukkit Project.
 * Created by Narimm on 19/11/2020.
 */
class PrismEntityEventsTest {

    ServerMock server;

    @BeforeEach
    void setUp() {
        server = MockBukkit.getOrCreateMock();
        MockBukkit.load(Prism.class);
    }

    @Test
    void testSetupSuccess() {
        assertTrue(MockBukkit.isMocked());
        assertTrue(server.getPluginManager().isPluginEnabled("Prism"));

    }

    @AfterEach
    void tearDown() {
    }
}