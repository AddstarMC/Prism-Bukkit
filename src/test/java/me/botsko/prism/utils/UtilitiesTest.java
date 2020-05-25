package me.botsko.prism.utils;

import be.seeseemelk.mockbukkit.MockBukkit;
import be.seeseemelk.mockbukkit.ServerMock;
import me.botsko.prism.utils.block.Utilities;
import org.bukkit.Material;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertTrue;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 23/05/2020.
 */
public class UtilitiesTest {

    private ServerMock server;

    @Before
    public void setUp() {
        server = MockBukkit.mock();
    }
    
    @Test
    public void testAreBlockIdsSameCoreItem() {
        Material m1 = Material.DIRT;
        Material m2 = Material.DIRT;
        assertTrue(Utilities.areBlockIdsSameCoreItem(m1,m2));
        m1 = Material.GRASS_BLOCK;
        assertTrue(Utilities.areBlockIdsSameCoreItem(m1,m2));
    }


}