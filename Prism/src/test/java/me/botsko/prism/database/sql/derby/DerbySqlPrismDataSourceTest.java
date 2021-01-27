package me.botsko.prism.database.sql.derby;

import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.testHelpers.TestPrismDataSource;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.Test;

/**
 * Created for the Prism-Bukkit Project.
 *
 * @author Narimm on
 * 27/01/2021.
 */
class DerbySqlPrismDataSourceTest {

    @Test
    void setupDatabase() {
        PrismLogHandler handler = new PrismLogHandler();
        ActionRegistry actionRegistry = new ActionRegistry();
        ConfigurationSection section = new MemoryConfiguration();
        section.set("type","derby");
        DerbySqlPrismDataSource source = new TestPrismDataSource(section);
    }
}