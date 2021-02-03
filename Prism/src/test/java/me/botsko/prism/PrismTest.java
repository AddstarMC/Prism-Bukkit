package me.botsko.prism;

import be.seeseemelk.mockbukkit.ServerMock;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.testHelpers.TestHelper;
import me.botsko.prism.utils.IntPair;
import me.botsko.prism.utils.MaterialAliases;
import org.bukkit.Material;
import org.bukkit.entity.EntityType;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collection;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 22/11/2020.
 */
public class PrismTest {

    static boolean integrationTesting = false;
    private static ServerMock server;
    private static TestHelper helper;

    @BeforeAll
    static void setUpAll() {
        helper = new TestHelper();
        server = helper.setup();
        if (Prism.getPrismDataSource().getDataSource() != null) {
            integrationTesting = true;
        }
    }

    @AfterAll
    static void afterAll() {
        TestHelper.shutdownHelper(helper);
    }

    @BeforeEach
    void setUp() {
        server.clearRecipes();
        server.getPluginManager().clearEvents();
        server.getScheduler().performOneTick();
    }

    @Test
    void setDebug() {
        Prism.setDebug(false);
        assertFalse(Prism.isDebug());
        Prism.setDebug(true);
        assertTrue(Prism.isDebug());
    }

    @Test
    void getIgnore() {
        assertNotNull(Prism.getIgnore());
        assertFalse(Prism.getIgnore().event("craft-item"));
    }

    @Test
    void getParameters() {
        assertNotNull(Prism.getParameters());
        assertNotNull(Prism.getParameter("radius"));
    }

    @Test
    void getIllegalBlocks() {
        assertNotNull(Prism.getIllegalBlocks());
        assertTrue(Prism.getIllegalBlocks().contains(Material.LAVA));
    }

    @Test
    void getIllegalEntities() {
        assertNotNull(Prism.getIllegalEntities());
        assertTrue(Prism.getIllegalEntities().contains(EntityType.CREEPER));
    }

    @Test
    void getMaterialAliases() {
        MaterialAliases aliases = Prism.getItems();
        assertNotNull(aliases);
        Collection<IntPair> ids = aliases.materialToAllIds(Material.DIRT);
        assertNotNull(ids);

    }

    @Test
    void getAlertedOres() {
        //assertEquals(TextColor.fromCSSHexString("#ffe17d"), Prism.getAlertedOres().get(Material.GOLD_ORE));
    }

    @Test
    void getHandlerRegistry() {
        assertNotNull(Prism.getHandlerRegistry());
        BlockAction action = Prism.getHandlerRegistry().create(BlockAction.class);
        assertNotNull(action.getTimeSince());
        assertNotNull(action.getDisplayDate());
    }

    @Test
    void getPrismVersion() {
        assertEquals(Prism.getInstance().getPrismVersion().substring(0, 5), "2.1.8");
    }
}