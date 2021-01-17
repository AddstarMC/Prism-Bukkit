package me.botsko.prism;

import be.seeseemelk.mockbukkit.ServerMock;
import me.botsko.prism.testHelpers.TestHelper;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 22/11/2020.
 */
public class PrismTest {

    static ServerMock server;
    static boolean integrationTesting = false;

    @BeforeAll
    static void setUpAll() {
        server = TestHelper.setup();
        if (Prism.getPrismDataSource().getDataSource() != null) {
            integrationTesting = true;
        }
    }

    @BeforeEach
    void setUp() {
        server.clearRecipes();
        server.getPluginManager().clearEvents();
        server.getScheduler().performOneTick();
    }
    @AfterAll
    static void tearDownFinal() {
        TestHelper.shutdown();
    }

    @Test
    void setDebug() {
        assertFalse(Prism.isDebug());
        Prism.setDebug(true);
        assertTrue(Prism.isDebug());
        server.getScheduler().waitAsyncTasksFinished();

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
    void getPrismVersion() {
        assertEquals(Prism.getInstance().getPrismVersion().substring(0,5),"2.1.8");
    }

    @Test
    void removeExpiredLocations() {
    }

}